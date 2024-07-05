import std/[
  algorithm,
  options,
  os,
  sequtils,
  strutils,
  sugar,
  tables
]
import experimental/cmdline
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

func cmpSuffix(a, b: string): int =
  ## Compare SemVer suffices
  # Trim metadata tag away
  let
    a = split(a, '+', maxsplit = 1)[0]
    b = split(b, '+', maxsplit = 1)[0]

  # Version with pre-release suffix is "older" than the one without
  result = cmp(ord(a == ""), ord(b == ""))
  if result != 0: return

  # Drop the leading `-` and split into components
  let
    aComponents = split(a[1..^1], '.')
    bComponents = split(b[1..^1], '.')

  func cmpComponent(a, b: string): int =
    let
      aNum =
        try: parseUInt(a)
        except ValueError: high uint
      bNum =
        try: parseUInt(b)
        except ValueError: high uint

    # This handles:
    # - both component being numeric
    # - only one of the component is numeric, in which case it's smaller than
    #   one with non-numeric values
    result = cmp(aNum, bNum)
    if result != 0:
      result = cmp(a, b)

  # Component-wise comparison
  for idx in 0 ..< min(aComponents.len, bComponents.len):
    result = cmpComponent(aComponents[idx], bComponents[idx])
    if result != 0: return

  # If all matching components are equal, then the one with more components is larger
  result = cmp(aComponents.len, bComponents.len)

func cmp(a, b: Version): int =
  ## Compare compiler version based on SemVer rules
  result = cmp(a.major, b.major)
  if result != 0: return
  result = cmp(a.minor, b.minor)
  if result != 0: return
  result = cmp(a.patch, b.patch)
  if result != 0: return
  result = cmpSuffix(a.suffix, b.suffix)

proc nextCommand(versionFile: string, modifier: NextModifier): int =
  let current =
    try:
      nversion.parse readFile(versionFile).strip()
    except ValueError:
      stderr.writeLine "error: invalid version in version file: ", versionFile
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
    stderr.writeLine "error: the next version must not have a suffix"
    return 1

  if cmp(current, next) >= 0:
    stderr.writeLine "error: the next version ($1) is not newer than the current version ($2)" % [$next, $current]
    return 1

  echo "bumping version to ", $next
  writeFile(versionFile, $next & "\n")

proc sortCommand(reverse: bool): int =
  var versions: seq[Version]
  func addSorted(s: var seq[Version], elem: sink Version) =
    let target = s.lowerBound(elem) do (a, b: Version) -> int:
      if not reverse:
        -cmp(a, b)
      else:
        cmp(a, b)

    s.insert(elem, target)

  for version in stdin.lines():
    # Skip empty lines
    if version == "": continue
    versions.addSorted:
      try:
        nversion.parse(version)
      except ValueError:
        stderr.writeLine "error: invalid version: ", version
        return 1

  for version in versions.items():
    stdout.writeLine version

type
  Action {.pure.} = enum
    ## The action to be taken
    Help
    Next
    Sort

  Config = object
    case action: Action
    of Next:
      next: tuple[file: Option[string], modifier: NextModifier]
    of Help:
      help: tuple[path: seq[string]]
    of Sort:
      sort: tuple[reverse: bool]

const
  DefaultVersionFile = "compiler/version.txt"
    ## The default version file to operate on

proc parseCli(T: typedesc[NextModifier], input: string): NextModifier =
  case input
  of "major": NextModifier(kind: Major)
  of "minor": NextModifier(kind: Minor)
  of "patch": NextModifier(kind: Patch)
  else: NextModifier(kind: As, version: nversion.parse(input))

proc printHelp(cli: Cli, commandPath: openArray[string]): int =
  ## Print help message for a given command.
  var current = RootCommand
  for name in commandPath.items():
    let next = cli.commandWithName(current, name)
    if next.isNone:
      stderr.writeLine("error: unknown subcommand: ", name)
      return 1
    current = next.get

  stdout.writeLine(cli.help(current))

proc dispatch(config: Config, cli: Cli): int =
  ## Dispatches based on `config`. Returns the exitcode.
  case config.action
  of Help:
    result = printHelp(cli, config.help.path)
  of Next:
    result = nextCommand(
      config.next.file.get(otherwise = DefaultVersionFile),
      config.next.modifier,
    )
  of Sort:
    result = sortCommand(config.sort.reverse)

proc addWithHelpTo[T](
  b: sink CommandBuilder[T],
  cli: var Cli[T],
  command: CommandId,
): CommandId {.discardable.} =
  result = b.addTo(cli, command)
  cli.addHelpFlag(result, "help", "h")

proc main() =
  ## The CLI entrypoint and parser
  var cli = commandBuilder(Config)
    .name("versiontool")
    .describe("compiler version helper")
    .initCli()
  cli.addHelpFlag(RootCommand, "help", "h")

  let nextCmd = cli.commandBuilder()
    .name("next")
    .describe("bump compiler version")
    .parser((_, var c) => (c = Config(action: Next)))
    .addWithHelpTo(cli, RootCommand)
  cli.flagBuilder()
    .name("file")
    .alias("f")
    .describe("version file to modify [default: $#]" % DefaultVersionFile)
    .parser(string, (_, val, var c) => (c.next.file = some(val)))
    .addTo(cli, nextCmd)
  cli.positionalBuilder()
    .name("VERSION")
    .describe("version to bump to or one of: `major`, `minor`, `patch`")
    .parser(NextModifier, (val, var c) => (c.next.modifier = val))
    .addTo(cli, nextCmd)

  let sortCmd = cli.commandBuilder()
    .name("sort")
    .describe("sort compiler versions from stdin in descending order")
    .parser((_, var c) => (c = Config(action: Sort)))
    .addWithHelpTo(cli, RootCommand)
  cli.flagBuilder()
    .name("reverse")
    .alias("r")
    .describe("reverse sort order")
    .parser(bool, (_, val, var c) => (c.sort.reverse = val))
    .addTo(cli, sortCmd)

  let helpCmd = cli.commandBuilder()
    .name("help")
    .alias("h")
    .describe("show help for command")
    .parser((_, var c) => (c = Config(action: Help)))
    .addWithHelpTo(cli, RootCommand)
  cli.positionalBuilder()
    .name("COMMAND")
    .describe("command to show help for, or the main command if not specified")
    .optional()
    .catchAll()
    .parser(string, (val, var c) => c.help.path.add val)
    .addTo(cli, helpCmd)

  let config = cli.run()
  quit config.dispatch(cli)

when isMainModule: main()
