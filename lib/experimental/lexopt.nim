#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/[strutils, options]

## This module provides an user-driven command line lexer.
##
## Unlike `std/parseopt`, this lexer parses command line tokens as requested
## by the caller.
runnableExamples:
  import std/options

  type
    Opts = object
      paths: seq[string]
      color: bool
      output: string

  var lexer = initCmdLexer(["bin", "--color", "/", "-o", "outfile.txt"])
  var (kind, option) = lexer.next()
  var opts = Opts()
  while kind != cmdEnd:
    if (kind == cmdLong and option == "color") or (kind == cmdShort and option == "c"):
      opts.color = true
    elif (kind == cmdLong and option == "output") or (kind == cmdShort and option == "o"):
      let value = lexer.value()
      if value.isNone:
        quit "error: '--output' must be followed by a value"
      opts.output = value.get()
    elif kind == cmdValue:
      opts.paths.add option

    (kind, option) = lexer.next()

  doAssert opts.paths == ["/"]
  doAssert opts.color == true
  doAssert opts.output == "outfile.txt"

const ValueDelims* = {':', '='}
  ## Characters that delimit between the option and its value

type
  CmdlineKind* = enum
    cmdEnd ## End of command line
    cmdLong ## A long option (ie. `--opt`)
    cmdShort ## A short option (ie. `-o`)
    cmdValue ## A value that is not an option

  CmdLexer* = object
    cmdline: seq[string] ## The command line to parse
    index: int ## The index of `cmdline` to be processed
    valueIdx: int ## \
      ## For long options, the index of `ValueDelims`.
      ##
      ## For short options, the index of the next flag to be processed.
      ##
      ## Otherwise, it is unused.

  LexError* = object of CatchableError
    ## Errors occurred while lexing

  UnexpectedValueError* = object of LexError
    ## A value was provided but was not consumed
    opt*: string ## The option that a value was provided to
    value*: string ## The provided value

proc initCmdLexer*(args: sink seq[string]): CmdLexer =
  ## Creates a new `CmdLexer`. `args` should be the full command line as
  ## returned by `os.commandLineArgs()`, where the first item is the binary name.
  CmdLexer(index: 1, cmdline: args)

proc initCmdLexer*(args: openArray[string]): CmdLexer =
  ## Creates a new `CmdLexer`. `args` should be the full command line as
  ## returned by `os.commandLineArgs()`, where the first item is the binary name.
  CmdLexer(index: 1, cmdline: @args)

template current(l: CmdLexer): string =
  l.cmdline[l.index]

proc isLongOpt*(s: string): bool = s.startsWith("--")
  ## Returns whether `s` can be parsed as a long option.
proc isShortOpt*(s: string): bool = s.startsWith('-')
  ## Returns whether `s` can be parsed as a short option.
proc isDash(s: string): bool = s == "-"
proc isDashDash(s: string): bool = s == "--"

proc prefix*(kind: CmdlineKind): string =
  ## Return the prefix string for a given command line parameter kind
  case kind
  of cmdLong:
    "--"
  of cmdShort:
    "-"
  else:
    ""

func newUnexpectedValueError(opt, value: string): ref UnexpectedValueError =
  (ref UnexpectedValueError)(
    msg: "unexpected value '" & value & "' for '" & opt & "'",
    opt: opt,
    value: value
  )

proc next*(l: var CmdLexer): (CmdlineKind, string) =
  ## Consume the next option.
  ##
  ## The following section describes how various syntaxes are handled:
  ##
  ## 1. `--` and `-` are considered values.
  ## 2. `--foo:bar` and `--foo=bar` are treated as long options with key `foo`
  ##    and `bar` is considered the value of `foo`.
  ##
  ##    If this value is not consumed before the next call to `next()`, an error
  ##    will be raised. See below for more details.
  ## 3. `--foo` is treated as a long option with key `foo`.
  ## 4. `-abco` is treated as into `a`, `b`, `c`, `o` short options, returned
  ##    one per `next()` call.
  ## 5. `-abco:foo` and `-abco=foo` are treated as `a`, `b`, `c`, `o` short
  ##    options, with `foo` being the value of option `o`.
  ##
  ##    If this value is not consumed before the next call to `next()`, an error
  ##    will be raised. See below for more details
  ## 6. Everything else is considered values.
  ##
  ## If the previous option have an unconsumed value (eg. `value()` was not
  ## called for `--opt:foo`), `UnexpectedValueError` will be raised.
  type
    LexState {.pure.} = enum
      ## Current lexer state
      Value ## A value that is not an option
      ShortOpt ## A short option
      LongOpt ## A long option
      LongValue ## Value of a long option
      ShortValue ## Value of a short option
      Ended ## Nothing left

  let currentState =
    if l.index >= l.cmdline.len:
      LexState.Ended
    elif l.current.isLongOpt:
      if l.valueIdx > 0:
        LexState.LongValue
      elif not l.current.isDashDash:
        LexState.LongOpt
      else:
        LexState.Value
    elif l.current.isShortOpt:
      if l.valueIdx > 0 and l.current[l.valueIdx] in ValueDelims:
        LexState.ShortValue
      elif not l.current.isDash:
        LexState.ShortOpt
      else:
        LexState.Value
    else:
      LexState.Value

  case currentState
  of LongOpt:
    let delimIdx = l.current.find(ValueDelims)
    if delimIdx > 2:
      l.valueIdx = delimIdx
      result = (cmdLong, l.current[2..<delimIdx])
    else:
      result = (cmdLong, l.current[2..^1])
      inc l.index
  of ShortOpt:
    l.valueIdx = if l.valueIdx == 0: 1 else: l.valueIdx
    result = (cmdShort, $l.current[l.valueIdx])

    inc l.valueIdx
    if l.valueIdx >= l.current.len:
      l.valueIdx = 0
      inc l.index
  of Value:
    result = (cmdValue, l.current)
    inc l.index
  of LongValue:
    raise newUnexpectedValueError(l.current[0..<l.valueIdx], l.current[l.valueIdx + 1..^1])
  of ShortValue:
    raise newUnexpectedValueError("-" & $l.current[l.valueIdx - 1], l.current[l.valueIdx + 1..^1])
  of Ended:
    result = (cmdEnd, "")

proc value*(l: var CmdLexer, delimitedOnly = false): Option[string] =
  ## Consume the value for the current option, if any.
  ##
  ## The following section describes how various syntaxes are handled:
  ##
  ## 1. `--foo:bar` or `--foo=bar` yields `bar` if called after `next()`
  ##    returns `foo`.
  ## 2. `-abco:foo` or `-abco=foo` yields foo if called after `next()` returns
  ##    `o`.
  ## 3. If `delimitedOnly` is true, syntaxes below will not be considered.
  ## 4. `-abcooutput.txt` yields `output.txt` if called after `next()` returns
  ##    `o`.
  ## 5. The current parameter is treated as a value, regardless of whether it
  ##    can be considered as an option (ie. `--foo --bar` will yield `--bar` if
  ##    this procedure is called after `--foo`).
  if l.index < l.cmdline.len:
    # The current index is parsed and contains a value
    if l.valueIdx > 0:
      # valueIdx is ValueDelims for either `--foo:bar` or `-o:bar`
      if l.current[l.valueIdx] in ValueDelims:
        result = some(l.current[l.valueIdx + 1..^1])

      # This is a short option being parsed, return the remainder
      # as a value if it is valid
      elif not delimitedOnly:
        result = some(l.current[l.valueIdx..^1])

      # Do nothing otherwise
      else:
        return

      l.valueIdx = 0
      inc l.index

    # The current index is not parsed, treat it as a value if valid
    elif not delimitedOnly:
      result = some(l.current)
      inc l.index

proc zerothParam*(l: CmdLexer): string =
  ## Return the 0th value from the command line sequence. This is typically the
  ## command binary name. Should only be used for informational purposes as
  ## this value could be set freely by the caller.
  runnableExamples:
    let lexer = initCmdLexer(["foo", "--do-x"])
    doAssert lexer.zerothParam == "foo"

  if l.cmdline.len > 0:
    l.cmdline[0]
  else:
    ""

iterator remaining*(l: var CmdLexer): string =
  ## Consume the remaining unprocessed parameters.
  ##
  ## If the previous option returned from `next()` have a value that was not
  ## consumed by `value()`, `UnexpectedValueError` will be raised.
  ##
  ## As a special case, any short options within a bundle (ie. `-abcde`) that
  ## have not been processed by `next()` is considered the value of the last
  ## short option returned and will raise `UnexpectedValueError`.
  if l.valueIdx > 0:
    if l.current.isLongOpt:
      raise newUnexpectedValueError(l.current[0..<l.valueIdx], l.current[l.valueIdx + 1..^1])
    else:
      let value =
        if l.current[l.valueIdx] in ValueDelims:
          l.current[l.valueIdx + 1..^1]
        else:
          l.current[l.valueIdx..^1]

      raise newUnexpectedValueError("-" & $l.current[l.valueIdx - 1], value)

  while l.index < l.cmdline.len:
    yield move l.current
    inc l.index
