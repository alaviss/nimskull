import std/[math, os, parseopt, sequtils, strutils, tables]
import compiler/utils/nversion

type
  NextModifierKind {.pure.} = enum
    Major
    Minor
    Patch
    As

  NextModifier = object
    case kind: NextModifierKind
    of Major, Minor, Patch: discard
    of As: version: Version

proc releaseLt(a, b: Version): bool =
  ## Returns whether `a` is older than `b`, without considering suffices.
  a.major < b.major or
  (a.major == b.major and a.minor < b.minor) or
  (a.major == b.major and a.minor == b.minor and a.patch < b.patch)

proc nextCommand(versionFile: string, modifier: NextModifier): int =
  let
    current =
      try:
        nversion.parse readFile(versionFile).strip()
      except ValueError:
        stderr.writeLine "error: Invalid version in version file: ", versionFile
        return 1

  var next = current
  next.suffix = "" # Clear the suffix (if any)
  case modifier.kind
  of Major:
    inc next.major
    next.minor = 0
    next.patch = 0
  of Minor:
    inc next.minor
    next.patch = 0
  of Patch: inc next.patch
  of As: next = modifier.version

  if next.suffix != "":
    stderr.writeLine "error: The next version must not have a suffix"
    return 1

  if not current.releaseLt next:
    stderr.writeLine "error: The next version ($1) is not newer than the current version ($2)" % [$next, $current]
    return 1

  echo "bumping version to ", $next
  writeFile(versionFile, $next & "\n")

type
  Action {.pure.} = enum
    ## The action to be taken
    Unknown
    Help = "help"
    Next = "next"

  Flag {.pure.} = enum
    ## Flags passed via CLI
    Error ## Not a valid flag. This is used to store invalid flag from command line.
    Help = "help" ## -h, --help
    File = "file" ## -f, --file
    Major = "major" ## -m, --major
    Minor = "minor" ## -n, --minor
    Patch = "patch" ## -p, --patch
    As = "as" ## --as

  CliErrorKind {.pure.} = enum
    ## Errors during CLI parsing
    NoError ## No error occurred
    InvalidFlag = "Invalid flag `$1'" ##
      ## An invalid flag was passed, flags[Error] contain the flag
    InvalidCommand = "Invalid command `$1'" ##
      ## An invalid command was passed, args[0] contain the command
    TerminatorBeforeCommand = "No command found before `--'" ##
      ## A terminator stopped command parsing before a command was found
    FlagNeedValue = "A value must be given to flag `$1'"
      ## A flag that requires a value was passed without one, flags[Error]
      ## contain the flag

  CliInterpErrorKind {.pure.} = enum
    ## Errors during CLI interpretation. This is a format string storage for
    ## the most part.
    FlagInvalidValue = "`$1' is not a valid value for flag `$2'"
      ## An invalid value was passed to a flag.

  Cli = object
    flags: Table[Flag, string] ## Table of flags passed and their value
    args: seq[string] ## The non-flag arguments
    error: CliErrorKind ## The error found during argument parsing
    action: Action ## The action to be taken

const
  GlobalOptHelp = """
Global options:
  -h, --help            Print help for any subcommand.
"""

  MainHelp = """
Usage: $app <command> [args]...

Commands:
  next  Bump the version in a version file
  help  Display help for any subcommand

$globalOpt
"""

  NextHelp = """
Usage: $app next [options]

Bump the version file according to the provided options. Default to bumping the
patch version.

The option --major, --minor, --patch and --as are mutually exclusive. An error
will be raised if more than one was passed.

Options:
  -m, --major           Bump the major version.
  -n, --minor           Bump the minor version.
  -p, --patch           Bump the patch version (default).
  --as:<version>        Bump to the absolute version. Must be higher than the
                        current version and must not be a pre-release.
  -f=<version.txt>,     Specify the version file to modify. Defaults to
  --file=<version.txt>  compiler/version.txt relative to the current directory.

$globalOpt
"""

  HelpHelp = """
Usage: $app help [options] [subcommand]

Print help text for the given subcommand, or the main help if no command nor
options were given.

$globalOpt
"""

  DefaultVersionFile = "compiler/version.txt"
    ## The default version file to operate on

proc printHelp(action: Action) =
  ## Print help message for `action`.
  let defaultHelpFormat = [
    "app", getAppFilename().lastPathPart(),
    "globalOpt", GlobalOptHelp
  ]
  case action
  of Unknown:
    stdout.write(MainHelp % defaultHelpFormat)
  of Action.Help:
    stdout.write(HelpHelp % defaultHelpFormat)
  of Next:
    stdout.write(NextHelp % defaultHelpFormat)

proc onlyFlags(cli: Cli, flags: set[Flag]): bool =
  ## Check and make sure only flags in `flags` are set in `cli`.
  result = true
  for flag, _ in cli.flags.pairs:
    if flag notin flags:
      return false

proc dispatch(cli: Cli): int =
  ## Dispatches based on `cli`. Returns the exitcode.
  case cli.error
  of InvalidFlag, FlagNeedValue:
    stderr.writeLine("error: ", $cli.error % cli.flags[Error])
    printHelp(cli.action)
    result = 1
  of InvalidCommand:
    stderr.writeLine("error: ", $InvalidCommand % cli.args[0])
    printHelp(Unknown)
    result = 1
  of TerminatorBeforeCommand:
    stderr.writeLine("error: ", $TerminatorBeforeCommand)
    printHelp(Unknown)
    result = 1
  of NoError:
    # If help was requested
    if Flag.Help in cli.flags:
      # Print help for action
      printHelp(cli.action)

    # Otherwise handle the actions
    else:
      case cli.action
      of Unknown:
        # No action was specified, print main help then set failure
        printHelp(Unknown)
        result = 1
      of Action.Help:
        if not cli.onlyFlags({}):
          printHelp(cli.action)
          return 1
        # If there are no command, print main help
        if cli.args.len == 0:
          printHelp(Unknown)
        else:
          let helpSubcommand = parseEnum[Action](cli.args[0], Unknown)
          # If the subcommand is invalid, print main help then set failure
          if helpSubcommand == Unknown:
            stderr.writeLine("error: ", $InvalidCommand % cli.args[0])
            printHelp(Unknown)
            result = 1
          else:
            printHelp(helpSubcommand)
      of Next:
        const ModifierFlags = {Flag.Major, Flag.Minor, Flag.Patch, Flag.As}
        if not cli.onlyFlags({Flag.File} + ModifierFlags):
          printHelp(cli.action)
          return 1

        let setModifiers = toSeq(cli.flags.keys()).mapIt(ord(it in ModifierFlags)).sum()
        if setModifiers > 1:
          stderr.writeLine "error: --major, --minor, --patch and --as are mutually exclusive"
          return 1

        let modifier =
          if Flag.Major in cli.flags:
            NextModifier(kind: NextModifierKind.Major)
          elif Flag.Minor in cli.flags:
            NextModifier(kind: NextModifierKind.Minor)
          elif Flag.Patch in cli.flags:
            NextModifier(kind: NextModifierKind.Patch)
          elif Flag.As in cli.flags:
            let nextVer =
              try:
                nversion.parse(cli.flags[Flag.As])
              except ValueError:
                stderr.writeLine "error: version $1 to bump to is invalid" % cli.flags[Flag.As]
                return 1
            NextModifier(
              kind: NextModifierKind.As,
              version: nextVer
            )
          else:
            NextModifier(kind: NextModifierKind.Patch)

        let versionFile = cli.flags.getOrDefault(Flag.File, DefaultVersionFile)
        if cli.args.len == 0:
          result = nextCommand(versionFile, modifier)
        else:
          # Any args was given, print the help text and set failure.
          printHelp(cli.action)
          result = 1

proc main() =
  ## The CLI entrypoint and parser
  var
    cliParser = initOptParser(
      getExecArgs(),
      shortNoVal = {'h'},
      longNoVal = @["--help"],
      allowWhitespaceAfterColon = false
    )

    cli: Cli

  for kind, key, val in cliParser.getopt():
    case kind
    of cmdArgument:
      # If no action have been specified
      if cli.action == Unknown:
        cli.action = parseEnum[Action](key, Unknown)
        # If the action was invalid
        if cli.action == Unknown:
          # Stop parsing here, this is an invalid token
          cli.error = InvalidCommand
          cli.args = @[key]
          break

      # Otherwise collect the arguments
      else:
        cli.args.add key
    of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
        cli.flags[Flag.Help] = ""
      of "major", "m":
        cli.flags[Flag.Major] = ""
      of "minor", "n":
        cli.flags[Flag.Minor] = ""
      of "patch", "p":
        cli.flags[Flag.Patch] = ""
      of "as":
        if val.len == 0:
          cli.error = FlagNeedValue
          cli.flags[Flag.Error] = key
          break

        cli.flags[Flag.As] = val
      of "file", "f":
        if val.len == 0:
          cli.error = FlagNeedValue
          cli.flags[Flag.Error] = key
          break

        cli.flags[Flag.File] = val
      of "":
        if cli.action == Unknown:
          cli.error = TerminatorBeforeCommand

        cli.args.add cliParser.remainingArgs
        break
      else:
        cli.error = InvalidFlag
        cli.flags[Flag.Error] = key
        break
    of cmdEnd:
      discard "nothing to do here"

  quit cli.dispatch()

when isMainModule: main()
