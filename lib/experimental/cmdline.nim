#
#
#                NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/hashes
import std/options
import std/strutils
import std/tables
import std/typetraits

import std/private/containers
import lexopt

type
  Cmdline* = object
    ## A command line parser
    flag: Table[string, Id] ## Lookup mapping of flag names to Id
    value: Table[Id, Value] ## Lookup mapping of Id to values
    name: Store[Id, string] ## Canonical names for all Id
    usage: Store[Id, string] ## Canonical usage for Ids

  ParseResult* = object
    ## Command line parsing result
    value: Table[Id, Value] ## Lookup of Id to parsed Values
    arg0*: string ## The zeroth argument of a command line, usually the program name
    remaining*: seq[string] ## All non-flags that were not considered

  ValueKind {.pure.} = enum
    Bool = "bool"
    String = "string"
    Int = "int"
    Float = "float"

  Value = object
    case kind: ValueKind
    of Bool:
      boolVal: bool
    of String:
      strVal: string
    of Int:
      intVal: int
    of Float:
      floatVal: float

  ParseError* = object of CatchableError
    ## An error during command line parsing
    result*: ParseResult ## The parse result upto the error

  FlagError* = object of ParseError
    ## An error parsing flags
    flagName*: string ## Name of the flag causing the error
  UnknownFlagError* = object of FlagError
    ## The flag parsed was not recognized
  MissingValueError* = object of FlagError
    ## The flag parsed requires a value but was not provided
    flagId*: SomeFlag ## Handle to the flag
  InvalidValueError* = object of FlagError
    ## Invalid value passed to a flag
    ##
    ## The parent `ValueError` can be found in the `parent` field
    flagValue*: string ## The string value received
    flagId*: SomeFlag ## Handle to the flag

  HelpError* = object of ParseError
    ## Help was requested

  Id = distinct uint32
  Flag*[T] = distinct Id
    ## A flag
  SomeFlag* = distinct Id

proc hash(x: Id): Hash {.borrow.}
proc `==`(a, b: Id): bool {.borrow.}
proc `==`*(a, b: SomeFlag): bool {.borrow.}
template `==`*[T](a, b: Flag[T]): bool =
  bind `==`
  Id(a) == Id(b)

converter toId[T](flag: Flag[T]): Id = Id(flag)
converter toId(flag: SomeFlag): Id = Id(flag)

func initValue(val: bool): Value {.inline.} =
  Value(kind: Bool, boolVal: val)

func initValue(val: sink string): Value {.inline.} =
  Value(kind: String, strVal: val)

func initValue(val: int): Value {.inline.} =
  Value(kind: Int, intVal: val)

func initValue(val: float): Value {.inline.} =
  Value(kind: Float, floatVal: val)

func isToggle(val: Value): bool {.inline.} =
  val.kind == Bool

func parse(val: var Value, input: string) =
  case val.kind
  of Bool:
    val.boolVal = parseBool(input)
  of String:
    val.strVal = input
  of Int:
    val.intVal = parseInt(input)
  of Float:
    val.floatVal = parseFloat(input)

func addFlagImpl(cmdline: var Cmdline, name, usage: sink string, default: sink Value): Id =
  doAssert name.len > 0, "Flag name must not be empty"
  if name in cmdline.flag:
    raise newException(ValueError, "Flag '" & name & "' already exists")

  result = cmdline.name.add(name)
  discard cmdline.usage.add(usage)
  cmdline.flag[name] = result
  cmdline.value[result] = default

func flagWithName*(cmdline: Cmdline, name: string): Option[SomeFlag] =
  try: some(SomeFlag cmdline.flag[name])
  except KeyError: none SomeFlag

func nameOf*(cmdline: Cmdline, flag: Flag or SomeFlag): lent string =
  cmdline.name[flag]

func usageOf*(cmdline: Cmdline, flag: Flag or SomeFlag): lent string =
  cmdline.usage[flag]

func defaultValueOf*[T](cmdline: Cmdline, flag: Flag[T]): lent T =
  when T is bool:
    cmdline.value[flag].boolVal
  elif T is string:
    cmdline.value[flag].strVal
  elif T is int:
    cmdline.value[flag].intVal
  elif T is float:
    cmdline.value[flag].floatVal
  else:
    {.error: "Unsupported flag type " & $T.}

func defaultStringValueOf*(cmdline: Cmdline, flag: Flag or SomeFlag): string =
  let value = cmdline.value[flag]
  case value.kind
  of Bool:
    if value.boolVal:
      $value.boolVal
    else:
      ""
  of String:
    value.strVal
  of Int:
    if value.intVal != 0:
      $value.intVal
    else:
      ""
  of Float:
    if value.floatVal != 0:
      $value.floatVal
    else:
      ""

func placeholderOf*(cmdline: Cmdline, flag: Flag or SomeFlag): string =
  $cmdline.value[flag].kind

func isToggle*(cmdline: Cmdline, flag: Flag or SomeFlag): bool =
  cmdline.value[flag].isToggle

iterator flags*(cmdline: Cmdline): SomeFlag =
  for id, _ in cmdline.name.pairs:
    yield SomeFlag(id)

func flagsUsage*(cmdline: Cmdline): string =
  const help = ("--help", "print help")
  var lines: seq[(string, string)]
  var flagPad: int
  for flag in cmdline.flags:
    let dash = if cmdline.nameOf(flag).len == 1: "-" else: "--"
    let display =
      if not cmdline.isToggle(flag):
        dash & cmdline.nameOf(flag) & " " & cmdline.placeholderOf(flag)
      else:
        dash & cmdline.nameOf(flag)
    let defaultTrailer = block:
      let defStr = cmdline.defaultStringValueOf(flag)
      if defStr.len > 0:
        " [default: " & defStr & "]"
      else:
        ""

    flagPad = max(flagPad, display.len)
    lines.add (display, cmdline.usageOf(flag) & defaultTrailer)

  if cmdline.flagWithName("help").isNone:
    flagPad = max(flagPad, help[0].len)
    lines.add help

  flagPad.inc 2
  for (flag, usage) in lines.items:
    if result.len > 0:
      result.add "\n"
    result.add "  "
    result.add:
      flag.alignLeft:
        if usage.len > 0: flagPad else: 0
    result.add usage

func addFlag*[T: bool or string or int or float](cmdline: var Cmdline, name, usage: sink string, default: sink T): Flag[T] {.inline.} =
  Flag[T] cmdline.addFlagImpl(name, usage, initValue(default))

func collectRemaining(result: var ParseResult, lexer: var CmdLexer) =
  for arg in lexer.remaining:
    result.remaining.add arg

func newHelpError(pr: sink ParseResult): ref HelpError {.raises: [].} =
  (ref HelpError)(
    msg: "help requested",
    result: pr
  )

func newUnknownFlagError(flag: sink string, pr: sink ParseResult): ref UnknownFlagError {.raises: [].} =
  (ref UnknownFlagError)(
    msg: "unexpected flag '" & flag & "'",
    result: pr,
    flagName: flag
  )

func newMissingValueError(flag: sink string, id: SomeFlag, pr: sink ParseResult): ref MissingValueError {.raises: [].} =
  (ref MissingValueError)(
    msg: "missing value for flag '" & flag & "'",
    result: pr,
    flagName: flag,
    flagId: id
  )

func newInvalidValueError(parent: ref ValueError, flag: sink string, id: SomeFlag, value: sink string, pr: sink ParseResult): ref InvalidValueError {.raises: [].} =
  (ref InvalidValueError)(
    msg: "invalid value for flag '" & flag & "': " & value,
    result: pr,
    flagName: flag,
    flagValue: value,
    flagId: id,
    parent: parent
  )

func parse*(cmdline: Cmdline, args: sink seq[string]): ParseResult {.raises: [ParseError].} =
  var lexer = initCmdLexer(args)
  result.arg0 = lexer.zerothParam
  result.value = cmdline.value

  try:
    while (let (kind, option) = lexer.next(); kind != cmdEnd):
      case kind
      of cmdLong, cmdShort:
        let flagId =
          try: cmdline.flag[option]
          except KeyError:
            result.collectRemaining lexer
            raise
              case option
              of "help", "h":
                newHelpError(move result)
              else:
                newUnknownFlagError(kind.prefix & option, move result)

        let value =
          try:
            if result.value[flagId].isToggle:
              let valOpt = lexer.value(delimitedOnly = true)
              valOpt.get("true")
            else:
              let valOpt = lexer.value()
              if valOpt.isNone:
                result.collectRemaining lexer
                raise newMissingValueError(kind.prefix & option, SomeFlag flagId, move result)

              valOpt.unsafeGet()
          except KeyError: raise newException(Defect, "unreachable")

        try: result.value[flagId].parse(value)
        except ValueError as e:
          result.collectRemaining lexer
          raise newInvalidValueError(e, kind.prefix & option, SomeFlag flagId, value, move result)
        except KeyError:
          raise newException(Defect, "unreachable")

      of cmdValue:
        if option != "--":
          result.remaining.add option
        else:
          result.collectRemaining lexer

      of cmdEnd:
        doAssert false, "unreachable!"
  except UnexpectedValueError:
    doAssert false, "unreachable!"

func parse*(cmdline: Cmdline, args: openArray[string]): ParseResult {.inline.} =
  parse(cmdline, @args)

func `[]`*[T](pr: ParseResult, flag: Flag[T]): T =
  when T is bool:
    pr.value[Id flag].boolVal
  elif T is string:
    pr.value[Id flag].strVal
  elif T is int:
    pr.value[Id flag].intVal
  elif T is float:
    pr.value[Id flag].floatVal
  else:
    {.error: "Unsupported type: " & $T.}

func `[]`*[T](pr: var ParseResult, flag: Flag[T]): var T =
  when T is bool:
    pr.value[Id flag].boolVal
  elif T is string:
    pr.value[Id flag].strVal
  elif T is int:
    pr.value[Id flag].intVal
  elif T is float:
    pr.value[Id flag].floatVal
  else:
    {.error: "Unsupported type: " & $T.}
