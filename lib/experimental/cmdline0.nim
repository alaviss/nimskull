#
#
#         NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/hashes
import std/options
import std/os
import std/parseopt
import std/sequtils
import std/sets
import std/strutils
import std/tables
import std/typetraits

type
  CmdCustomValBase = ref object of RootObj
  CmdCustomVal[T] = ref object of CmdCustomValBase
    value: T

  CmdValueKind {.pure.} = enum
    Bool
    String
    Int
    Float
    Custom

  CmdValue = object
    case kind: CmdValueKind
    of Bool:
      boolVal: bool
    of String:
      strVal: string
    of Int:
      intVal: int
    of Float:
      floatVal: float
    of Custom:
      customVal: CmdCustomValBase

  CmdValueDesc = object
    case kind: CmdValueKind
    of Bool:
      boolVal: Option[bool]
    of String:
      strVal: Option[string]
    of Int:
      intVal: Option[int]
    of Float:
      floatVal: Option[float]
    of Custom:
      parseCustom: proc(current: CmdCustomValBase, input: string): CmdCustomValBase
      customVal: CmdCustomValBase
      typeName: string

  CmdElementKind {.pure.} = enum
    Command
    Flag
    MultiFlag
    Positional
    OptionalPositional
    TrailingPositional
    TrailingOptionalPositional

  CmdFlagKind = range[CmdElementKind.Flag..CmdElementKind.MultiFlag]
  CmdPositionalKind = range[CmdElementKind.Positional..CmdElementKind.TrailingOptionalPositional]

  Cmdline* {.requiresInit.} = object
    positional: Table[Command, seq[Id]]
    flag: Table[Command, Table[string, Id]]
    cmd: Table[Command, Table[string, Command]]
    valueDesc: Table[Id, CmdValueDesc]
    valueName: Table[Id, string]
    parent: seq[Id]
    name: seq[string]
    kind: seq[CmdElementKind]
    description: seq[string]

  ParseResult* = object
    name: Table[Id, string]
    kind: Table[Id, CmdElementKind]
    positional: seq[Id]
    value: Table[Id, seq[CmdValue]]
    command: seq[Command]

  ParseFlags* {.pure.} = enum
    RaiseError

  EmptyError* = object of ValueError
  ParseError* = object of CatchableError
    result*: ParseResult

  FlagError* = object of ParseError
    parseError*: ref CatchableError
    flagName*: string
    flagValue*: string
    flagId: Id
    command*: Command
    errorKind*: FlagErrorKind

  PositionalError* = object of ParseError
    parseError*: ref CatchableError
    posValue*: string
    posName*: string
    position*: int
    posId: Id
    command*: Command
    errorKind*: PositionalErrorKind

  UnknownSubcommandError* = object of ParseError
    subcommand*: string
    command*: Command

  HelpError* = object of ParseError
    command*: Command

  FlagErrorKind* {.pure.} = enum
    InvalidValue
    NoValue
    UnknownFlag

  PositionalErrorKind* {.pure.} = enum
    InvalidValue
    Missing
    Unexpected

  Id = distinct uint32
  ValueId = distinct uint32
  Flag*[T] = distinct Id
  MultiFlag*[T] = distinct Id
  Positional*[T] = distinct Id
  TrailingPositional*[T] = distinct Id
  Command* = distinct Id

const
  RootId = Id(0)
  MainCommand = Command(RootId)
  InvalidId = Id(not 0u32)
  InvalidValueId = ValueId(not 0u32)

  ValidNameChars* = {'0'..'9', 'a'..'z', 'A'..'Z', '-', '_', '.'}

proc hash(id: Id): Hash {.borrow.}
proc hash(id: Command): Hash {.borrow.}
proc hash(id: ValueId): Hash {.borrow.}
proc `==`(a, b: Id): bool {.borrow.}
proc `==`(a, b: ValueId): bool {.borrow.}
proc `==`*(a, b: Command): bool {.borrow.}
converter toId[T](flag: Flag[T]): Id = Id(flag)
converter toId(command: Command): Id = Id(command)

proc initCmdline*(program: string, description: string): Cmdline =
  Cmdline(
    name: @[program],
    description: @[description],
    parent: @[InvalidId],
    kind: @[CmdElementKind.Command],

    positional: default(typeof Cmdline.positional),
    flag: default(typeof Cmdline.flag),
    cmd: default(typeof Cmdline.cmd),
    valueDesc: default(typeof Cmdline.valueDesc),
    valueName: default(typeof Cmdline.valueName),
  )

proc parseValue(T: typedesc[bool], input: string): bool =
  case input.toLowerAscii()
  of "on", "1", "yes", "y", "true", "":
    result = true
  of "off", "0", "no", "n", "false":
    result = false
  else:
    raise newException(ValueError, "must be a boolean value")
proc parseValue(T: typedesc[string], input: string): string =
  input
proc parseValue(T: typedesc[int], input: string): int =
  if input == "":
    raise newException(EmptyError, "expected an integer")
  if input.len > 2:
    case input[0..<2]
    of "0x", "0X":
      parseHexInt(input)
    of "0o", "0O":
      parseOctInt(input)
    of "0b", "0B":
      parseBinInt(input)
    else:
      parseInt(input)
  else:
    parseInt(input)
proc parseValue(T: typedesc[float], input: string): float =
  if input == "":
    raise newException(EmptyError, "expected a floating point value")
  parseFloat(input)

proc checkName(name: string) =
  if name == "":
    raise newException(ValueError, "name must not be empty")
  if name[0] == '-':
    raise newException(ValueError, "name must not start with '-'")
  if not name.allIt(it in ValidNameChars):
    raise newException(ValueError, "name contains disallowed characters")

proc initValueDesc(T: typedesc[bool]): CmdValueDesc = CmdValueDesc(kind: Bool)
proc initValueDesc(T: typedesc[string]): CmdValueDesc = CmdValueDesc(kind: String)
proc initValueDesc(T: typedesc[int]): CmdValueDesc = CmdValueDesc(kind: Int)
proc initValueDesc(T: typedesc[float]): CmdValueDesc = CmdValueDesc(kind: Float)
proc initValueDesc[T](parser: proc(input: string): T): CmdValueDesc =
  proc parseCustom(current: CmdCustomValBase, input: string): CmdCustomValBase =
    CmdCustomVal[T](value: parser(input))

  CmdValueDesc(
    kind: Custom,
    typeName: $T,
    parseCustom: parseCustom
  )

proc initValueDesc(value: bool): CmdValueDesc = CmdValueDesc(kind: Bool, boolVal: some(value))
proc initValueDesc(value: string): CmdValueDesc = CmdValueDesc(kind: String, strVal: some(value))
proc initValueDesc(value: int): CmdValueDesc = CmdValueDesc(kind: Int, intVal: some(value))
proc initValueDesc(value: float): CmdValueDesc = CmdValueDesc(kind: Float, floatVal: some(value))
proc initValueDesc[T](value: T, parser: proc(input: string, current: T): T): CmdValueDesc =
  proc parseCustom(current: CmdCustomValBase, input: string): CmdCustomValBase =
    if current.isNil:
      CmdCustomVal[T](value: parser(input))
    else:
      let current = CmdCustomVal[T](current)
      current.value = parser(input, current.value)
      current

  CmdValueDesc(
    kind: Custom,
    typeName: $T,
    customVal: CmdCustomVal[T](value: value),
    parseCustom: parseCustom
  )

proc parse(desc: CmdValueDesc, input: string): CmdValue =
  case desc.kind
  of Bool:
    CmdValue(kind: Bool, boolVal: parseValue(bool, input))
  of String:
    CmdValue(kind: String, strVal: parseValue(string, input))
  of Int:
    CmdValue(kind: Int, intVal: parseValue(int, input))
  of Float:
    CmdValue(kind: Float, floatVal: parseValue(float, input))
  of Custom:
    CmdValue(kind: Custom, customVal: desc.customVal)

proc default(desc: CmdValueDesc): Option[CmdValue] =
  case desc.kind
  of Bool:
    desc.boolVal.map(
      proc (val: bool): CmdValue =
        CmdValue(kind: Bool, boolVal: val)
    )
  of String:
    desc.strVal.map(
      proc (val: string): CmdValue =
        CmdValue(kind: String, strVal: val)
    )
  of Int:
    desc.intVal.map(
      proc (val: int): CmdValue =
        CmdValue(kind: Int, intVal: val)
    )
  of Float:
    desc.floatVal.map(
      proc (val: float): CmdValue =
        CmdValue(kind: Float, floatVal: val)
    )
  of Custom:
    if desc.customVal.isNil:
      none CmdValue
    else:
      some CmdValue(kind: Custom, customVal: desc.customVal)

proc isFlag(cmdline: Cmdline, flag: Id): bool =
  flag.int in 0..cmdline.kind.len and
  cmdline.kind[flag.int] in {CmdElementKind.Flag, CmdElementKind.MultiFlag}

proc hasFlag(cmdline: Cmdline, command: Command, name: string): bool =
  command in cmdline.flag and name in cmdline.flag[command]

proc isCommand(cmdline: Cmdline, command: Id): bool =
  command.int in 0..cmdline.kind.len and
  cmdline.kind[command.int] == CmdElementKind.Command

proc hasCommand(cmdline: Cmdline, command: Command, name: string): bool =
  command in cmdline.cmd and name in cmdline.cmd[command]

proc hasCommands(cmdline: Cmdline, command: Command): bool =
  command in cmdline.cmd and cmdline.cmd[command].len > 0

proc hasPositionals(cmdline: Cmdline, command: Command): bool =
  command in cmdline.positional and cmdline.positional[command].len > 0

proc hasTrailingPositional(cmdline: Cmdline, command: Command): bool =
  command in cmdline.positional and cmdline.positional[command].len > 0 and
  cmdline.kind[cmdline.positional[command][^1].int] in {CmdElementKind.TrailingPositional, CmdElementKind.TrailingOptionalPositional}

proc hasOptionalLastPositional(cmdline: Cmdline, command: Command): bool =
  command in cmdline.positional and cmdline.positional[command].len > 0 and
  cmdline.kind[cmdline.positional[command][^1].int] in {CmdElementKind.OptionalPositional, CmdElementKind.TrailingOptionalPositional}

proc addFlagImpl(cmdline: var Cmdline, command: Command, name: string, description: string, valueDesc: CmdValueDesc, flagKind: CmdFlagKind): Id =
  doAssert cmdline.isCommand(command), "Object with id=$1 is not a command" % $command.int
  checkName name
  if cmdline.hasFlag(command, name):
    raise newException(ValueError):
      if command == MainCommand:
        "Flag '$1' already exists for the main command" % name
      else:
        "Flag '$1' already exists for the command: $2" % [name, cmdline.name[command.int]]

  result = Id(cmdline.name.len)
  cmdline.name.add name
  cmdline.kind.add flagKind
  cmdline.parent.add command.Id
  cmdline.description.add description
  if command notin cmdline.flag:
    cmdline.flag[command] = {name: result.Id}.toTable()
  else:
    cmdline.flag[command][name] = result.Id
  cmdline.valueDesc[result.Id] = valueDesc

proc addFlagAlias(cmdline: var Cmdline, flag: Id, name: string) =
  doAssert cmdline.isFlag(flag), "Object with id=$1 is not a flag" % $flag.int
  let command = Command(cmdline.parent[flag.int])
  checkName name
  if cmdline.hasFlag(command, name):
    raise newException(ValueError):
      if command == MainCommand:
        "Flag '$1' already exists for the main command" % name
      else:
        "Flag '$1' already exists for the command: $2" % [name, cmdline.name[command.int]]

  cmdline.flag[command][name] = flag

proc addFlag*[T](cmdline: var Cmdline, command: Command, name: string, default: T, description: string, parser: proc(input: string, current: T): T): Flag[T] {.inline.} =
  Flag[T] cmdline.addFlagImpl(command, name, description, initValueDesc(default, parser), CmdElementKind.Flag)

proc addFlag*(cmdline: var Cmdline, command: Command, name: string, default: bool, description: string): Flag[bool] {.inline.} =
  Flag[bool] cmdline.addFlagImpl(command, name, description, initValueDesc(default), CmdElementKind.Flag)

proc addFlag*(cmdline: var Cmdline, command: Command, name: string, default: string, description: string): Flag[string] {.inline.} =
  Flag[string] cmdline.addFlagImpl(command, name, description, initValueDesc(default), CmdElementKind.Flag)

proc addFlag*(cmdline: var Cmdline, command: Command, name: string, default: int, description: string): Flag[int] {.inline.} =
  Flag[int] cmdline.addFlagImpl(command, name, description, initValueDesc(default), CmdElementKind.Flag)

proc addFlag*(cmdline: var Cmdline, command: Command, name: string, default: float, description: string): Flag[float] {.inline.} =
  Flag[float] cmdline.addFlagImpl(command, name, description, initValueDesc(default), CmdElementKind.Flag)

proc addFlag*[T](cmdline: var Cmdline, name: string, default: T, description: string, parser: proc(input: string): T): Flag[T] {.inline.} =
  cmdline.addFlag(MainCommand, name, default, description, parser)

proc addFlag*(cmdline: var Cmdline, name: string, default: bool, description: string): Flag[bool] {.inline.} =
  cmdline.addFlag(MainCommand, name, default, description)

proc addFlag*(cmdline: var Cmdline, name: string, default: string, description: string): Flag[string] {.inline.} =
  cmdline.addFlag(MainCommand, name, default, description)

proc addFlag*(cmdline: var Cmdline, name: string, default: int, description: string): Flag[int] {.inline.} =
  cmdline.addFlag(MainCommand, name, default, description)

proc addFlag*(cmdline: var Cmdline, name: string, default: float, description: string): Flag[float] {.inline.} =
  cmdline.addFlag(MainCommand, name, default, description)

proc addMultiFlag*(cmdline: var Cmdline, command: Command, name: string, T: typedesc, description: string, parser: proc(input: string): T): MultiFlag[T] {.inline.} =
  MultiFlag[T] cmdline.addFlagImpl(command, name, description, initValueDesc(parser), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[bool], description: string): MultiFlag[bool] {.inline.} =
  MultiFlag[bool] cmdline.addFlagImpl(command, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[string], description: string): MultiFlag[string] {.inline.} =
  MultiFlag[string] cmdline.addFlagImpl(command, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[int], description: string): MultiFlag[int] {.inline.} =
  MultiFlag[int] cmdline.addFlagImpl(command, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[float], description: string): MultiFlag[float] {.inline.} =
  MultiFlag[float] cmdline.addFlagImpl(command, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, name: string, T: typedesc, description: string, parser: proc(input: string): T): MultiFlag[T] {.inline.} =
  MultiFlag[T] cmdline.addFlagImpl(MainCommand, name, description, initValueDesc(parser), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, name: string, T: typedesc[bool], description: string): MultiFlag[bool] {.inline.} =
  MultiFlag[bool] cmdline.addFlagImpl(MainCommand, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, name: string, T: typedesc[string], description: string): MultiFlag[string] {.inline.} =
  MultiFlag[string] cmdline.addFlagImpl(MainCommand, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, name: string, T: typedesc[int], description: string): MultiFlag[int] {.inline.} =
  MultiFlag[int] cmdline.addFlagImpl(MainCommand, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addMultiFlag*(cmdline: var Cmdline, name: string, T: typedesc[float], description: string): MultiFlag[float] {.inline.} =
  MultiFlag[float] cmdline.addFlagImpl(MainCommand, name, description, initValueDesc(T), CmdElementKind.MultiFlag)

proc addAlias*[T](cmdline: var Cmdline, flag: Flag[T] or MultiFlag[T], name: string) {.inline.} =
  cmdline.addFlagAlias(Id flag, name)

proc addCommand*(cmdline: var Cmdline, command: Command, name, description: string): Command =
  doAssert cmdline.isCommand(command), "Object with id=$1 is not a command" % $command.int
  checkName name
  if cmdline.hasPositionals(command):
    raise newException(ValueError):
      if command == MainCommand:
        "Main command cannot have both positionals and subcommands"
      else:
        "Command '$1' cannot have both positionals and subcommands" % cmdline.name[command.int]
  if cmdline.hasCommand(command, name):
    raise newException(ValueError):
      if command == MainCommand:
        "Subcommand '$1' already exists for the main command" % name
      else:
        "Subcommand '$1' already exists for the command: $2" % [name, cmdline.name[command.int]]
  result = Command(cmdline.name.len)

  cmdline.name.add name
  cmdline.kind.add CmdElementKind.Command
  cmdline.parent.add command
  cmdline.description.add description
  if command notin cmdline.cmd:
    cmdline.cmd[command] = {name: result}.toTable()
  else:
    cmdline.cmd[command][name] = result

proc addCommand*(cmdline: var Cmdline, name, description: string): Command {.inline.} =
  cmdline.addCommand(MainCommand, name, description)

proc addAlias*(cmdline: var Cmdline, command: Command, name: string) =
  doAssert cmdline.isCommand(command), "Object with id=$1 is not a command" % $command.int
  checkName name
  let parent = Command(cmdline.parent[command.int])
  if cmdline.hasCommand(parent, name):
    raise newException(ValueError):
      if command == MainCommand:
        "Subcommand '$1' already exists for the main command" % name
      else:
        "Subcommand '$1' already exists for the command: $2" % [name, cmdline.name[command.int]]

  cmdline.cmd[parent][name] = command

proc addPositionalImpl(cmdline: var Cmdline, command: Command, name: string, description: string, valueDesc: CmdValueDesc, posKind: CmdPositionalKind): Id =
  doAssert cmdline.isCommand(command), "Object with id=$1 is not a command" % $command.int
  checkName name
  if cmdline.hasCommands(command):
    raise newException(ValueError):
      if command == MainCommand:
        "Main command cannot have both positionals and subcommands"
      else:
        "Command '$1' cannot have both positionals and subcommands" % cmdline.name[command.int]
  if cmdline.hasTrailingPositional(command):
    raise newException(ValueError):
      if command == MainCommand:
        "Main command already have a trailing positional argument, no additional positionals can be added"
      else:
        "Command '$1' already have a trailing positional argument, no additional positionals can be added" % cmdline.name[command.int]
  if cmdline.hasOptionalLastPositional(command) and posKind in {CmdElementKind.Positional, CmdElementKind.TrailingPositional}:
    raise newException(ValueError):
      if command == MainCommand:
        "Main command already have an optional positional argument, no additional required positionals can be added"
      else:
        "Command '$1' already have an optional positional argument, no additional required positionals can be added" % cmdline.name[command.int]

  result = Id(cmdline.name.len)
  cmdline.name.add name
  cmdline.kind.add posKind
  cmdline.parent.add command.Id
  cmdline.description.add description
  if command notin cmdline.positional:
    cmdline.positional[command] = @[result]
  else:
    cmdline.positional[command].add result
  cmdline.valueDesc[result.Id] = valueDesc

proc addPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc, description: string, parser: proc(input: string): T, required = false): Positional[T] {.inline.} =
  Positional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(parser), if required: Positional else: OptionalPositional)

proc addPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[bool], description: string, required = false): Positional[T] {.inline.} =
  Positional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: Positional else: OptionalPositional)

proc addPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[string], description: string, required = false): Positional[T] {.inline.} =
  Positional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: Positional else: OptionalPositional)

proc addPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[int], description: string, required = false): Positional[T] {.inline.} =
  Positional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: Positional else: OptionalPositional)

proc addPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[float], description: string, required = false): Positional[T] {.inline.} =
  Positional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: Positional else: OptionalPositional)

proc addPositional*(cmdline: var Cmdline, name: string, T: typedesc, description: string, parser: proc(input: string): T, required = false): Positional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, parser, required)

proc addPositional*(cmdline: var Cmdline, name: string, T: typedesc[bool], description: string, required = false): Positional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addPositional*(cmdline: var Cmdline, name: string, T: typedesc[string], description: string, required = false): Positional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addPositional*(cmdline: var Cmdline, name: string, T: typedesc[int], description: string, required = false): Positional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addPositional*(cmdline: var Cmdline, name: string, T: typedesc[float], description: string, required = false): Positional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addTrailingPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc, description: string, parser: proc(input: string): T, required = false): TrailingPositional[T] {.inline.} =
  TrailingPositional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(parser), if required: TrailingPositional else: TrailingOptionalPositional)

proc addTrailingPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[bool], description: string, required = false): TrailingPositional[T] {.inline.} =
  TrailingPositional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: TrailingPositional else: TrailingOptionalPositional)

proc addTrailingPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[string], description: string, required = false): TrailingPositional[T] {.inline.} =
  TrailingPositional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: TrailingPositional else: TrailingOptionalPositional)

proc addTrailingPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[int], description: string, required = false): TrailingPositional[T] {.inline.} =
  TrailingPositional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: TrailingPositional else: TrailingOptionalPositional)

proc addTrailingPositional*(cmdline: var Cmdline, command: Command, name: string, T: typedesc[float], description: string, required = false): TrailingPositional[T] {.inline.} =
  TrailingPositional[T] cmdline.addPositionalImpl(command, name, description, initValueDesc(T), if required: TrailingPositional else: TrailingOptionalPositional)

proc addTrailingPositional*(cmdline: var Cmdline, name: string, T: typedesc, description: string, parser: proc(input: string): T, required = false): TrailingPositional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, parser, required)

proc addTrailingPositional*(cmdline: var Cmdline, name: string, T: typedesc[bool], description: string, required = false): TrailingPositional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addTrailingPositional*(cmdline: var Cmdline, name: string, T: typedesc[string], description: string, required = false): TrailingPositional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addTrailingPositional*(cmdline: var Cmdline, name: string, T: typedesc[int], description: string, required = false): TrailingPositional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addTrailingPositional*(cmdline: var Cmdline, name: string, T: typedesc[float], description: string, required = false): TrailingPositional[T] {.inline.} =
  cmdline.addPositional(MainCommand, name, T, description, required)

proc addElementFrom(parseResult: var ParseResult, cmdline: Cmdline, id: Id) =
  doAssert id.int in 0..cmdline.name.high
  parseResult.name[id] = cmdline.name[id.int]
  parseResult.kind[id] = cmdline.kind[id.int]

proc newUnknownSubcommandError(parseResult: ParseResult, currentCommand: Command, subcommand: string): ref UnknownSubcommandError =
  (ref UnknownSubcommandError)(
    msg: if subcommand.len > 0: "unknown command '$1'" % subcommand else: "expected command",
    result: parseResult,
    subcommand: subcommand,
    command: currentCommand
  )

proc newInvalidPositionalError(
  parseResult: ParseResult,
  currentCommand: Command,
  parseError: ref CatchableError,
  id: Id,
  value: string,
  position: int
): ref PositionalError =
  let parseErrMsg = if parseError.isNil: "" else: ": " & parseError.msg
  let name = parseResult.name[id]
  let msg = "invalid value '$1' for parameter <$2>$3" % [value, parseResult.name[id], parseErrMsg]
  (ref PositionalError)(
    msg: msg,
    result: parseResult,
    posName: name,
    posValue: value,
    position: position,
    posId: id,
    command: currentCommand,
    errorKind: PositionalErrorKind.InvalidValue
  )
proc newUnknownPositionalError(
    parseResult: ParseResult,
    currentCommand: Command,
    value: string,
    position: int
): ref PositionalError =
  let msg = "unexpected argument '$1' found" % value
  (ref PositionalError)(
    msg: msg,
    result: parseResult,
    posValue: value,
    position: position,
    posId: InvalidId,
    command: currentCommand,
    errorKind: Unexpected
  )
proc newMissingPositionalError(
  parseResult: ParseResult,
  currentCommand: Command,
  id: Id,
  position: int
): ref PositionalError =
  let name = parseResult.name[id]
  let msg = "missing argument '<$1>'" % name
  (ref PositionalError)(
    msg: msg,
    result: parseResult,
    position: position,
    posId: id,
    command: currentCommand,
    errorKind: Missing
  )
proc newUnknownFlagError(
  parseResult: ParseResult,
  currentCommand: Command,
  dash: string,
  name: string,
  value: string
): ref FlagError =
  let msg = "unexpected argument '$1$2:$3' found" % [dash, name, value]
  (ref FlagError)(
    msg: msg,
    result: parseResult,
    flagName: name,
    flagValue: value,
    flagId: InvalidId,
    command: currentCommand,
    errorKind: UnknownFlag
  )
proc newInvalidFlagValueError(
  parseResult: ParseResult,
  currentCommand: Command,
  parseError: ref CatchableError,
  id: Id,
  value: string
): ref FlagError =
  let parseErrMsg = if parseError.isNil: "" else: ": " & parseError.msg
  let name = parseResult.name[id]
  let dash = if name.len > 1: "--" else: "-"
  let msg = "invalid value '$1' for parameter '$2$3'$4" % [value, dash, parseResult.name[id], parseErrMsg]
  (ref FlagError)(
    msg: msg,
    result: parseResult,
    flagName: name,
    flagValue: value,
    flagId: id,
    command: currentCommand,
    errorKind: FlagErrorKind.InvalidValue
  )
proc newEmptyFlagValueError(
  parseResult: ParseResult,
  currentCommand: Command,
  parseError: ref CatchableError,
  id: Id
): ref FlagError =
  let parseErrMsg = if parseError.isNil: "" else: ": " & parseError.msg
  let name = parseResult.name[id]
  let dash = if name.len > 1: "--" else: "-"
  let msg = "missing value for parameter '$1$2'$3" % [dash, parseResult.name[id], parseErrMsg]
  (ref FlagError)(
    msg: msg,
    result: parseResult,
    flagName: name,
    flagId: id,
    command: currentCommand,
    errorKind: NoValue
  )

proc newHelpError(
  parseResult: ParseResult,
  currentCommand: Command
): ref HelpError =
  (ref HelpError)(
    msg: "received argument '--help'",
    result: parseResult,
    command: currentCommand
  )

proc findFlag(cmdline: Cmdline, command: Command, name: string): Id =
  doAssert cmdline.isCommand(command)
  result = InvalidId
  var command = command
  while command != InvalidId:
    if command in cmdline.flag and name in cmdline.flag[command]:
      return cmdline.flag[command][name]
    command = cmdline.parent[command.int].Command

iterator flags(cmdline: Cmdline, command: Command): Id =
  var command = command
  while command != InvalidId:
    if command in cmdline.flag:
      for id in cmdline.flag[command].values():
        yield id
    command = cmdline.parent[command.int].Command

proc parse*(cmdline: Cmdline, args: openArray[string] = commandLineParams()): ParseResult =
  var shortNoVal = {'h'}
  var longNoVal = ["", "help"].toSet
  var currentCommand = MainCommand
  var optParser: OptParser = initOptParser(args)

  result.addElementFrom cmdline, currentCommand

  block parse:
    while true:
      block thisCommand:
        if currentCommand in cmdline.flag:
          for flag, id in cmdline.flag[currentCommand].pairs:
            if cmdline.valueDesc[id].kind == CmdValueKind.Bool:
              if flag.len == 1:
                shortNoVal.incl flag[0]
              else:
                longNoVal.incl flag
            else:
              if flag.len == 1:
                shortNoVal.excl flag[0]
              else:
                longNoVal.excl flag

        optParser = initOptParser(optParser.remainingArgs, shortNoVal = shortNoVal, longNoVal = longNoVal.toSeq)

        var positional = 0
        template parsePositional(input: string): untyped =
          let inp = input
          if currentCommand in cmdline.positional and positional in 0..cmdline.positional[currentCommand].high:
            let posId = cmdline.positional[currentCommand][positional]
            result.addElementFrom cmdline, posId

            let desc = cmdline.valueDesc[posId]
            let value =
              try:
                desc.parse(inp)
              except ValueError as e:
                raise newInvalidPositionalError(result, currentCommand, e, posId, inp, positional)

            if cmdline.kind[posId.int] notin {CmdElementKind.TrailingPositional, CmdElementKind.TrailingOptionalPositional}:
              result.value[posId] = @[value]
              inc positional
            else:
              result.value[posId].add value
          else:
            raise newUnknownPositionalError(result, currentCommand, inp, positional)

        for kind, key, val in optParser.getopt():
          case kind
          of cmdArgument:
            if currentCommand in cmdline.cmd:
              if key in cmdline.cmd[currentCommand]:
                currentCommand = cmdline.cmd[currentCommand][key]
                result.addElementFrom(cmdline, currentCommand)
                result.command.add currentCommand
                break thisCommand # Move on to process the next command

              raise newUnknownSubcommandError(result, currentCommand, key)

            parsePositional(key)
          of cmdShortOption, cmdLongOption:
            if key == "":
              break
            let flagId = cmdline.findFlag(currentCommand, key)
            if flagId == InvalidId:
              case key
              of "h", "help":
                raise newHelpError(result, currentCommand)
              else:
                let dash = if kind == cmdShortOption: "-" else: "--"
                raise newUnknownFlagError(result, currentCommand, dash, key, val)
            result.addElementFrom cmdline, flagId
            let desc = cmdline.valueDesc[flagId]
            let value =
              try:
                desc.parse(val)
              except EmptyError as e:
                raise newEmptyFlagValueError(result, currentCommand, e, flagId)
              except ValueError as e:
                raise newInvalidFlagValueError(result, currentCommand, e, flagId, val)

            if cmdline.kind[flagId.int] == CmdElementKind.MultiFlag:
              result.value[flagId].add value
            elif flagId notin result.value:
              result.value[flagId] = @[value]
            else:
              result.value[flagId][^1] = value
          of cmdEnd: doAssert false, "unreachable"

        for arg in optParser.remainingArgs: parsePositional(arg)

        # We are done
        break parse

  if currentCommand in cmdline.cmd:
    raise newUnknownSubcommandError(result, currentCommand, "")

  if currentCommand in cmdline.positional:
    for position in 0..cmdline.positional[currentCommand].high:
      let posId = cmdline.positional[currentCommand][position]
      if posId notin result.value:
        if cmdline.kind[posId.int] in {CmdElementKind.Positional, CmdElementKind.TrailingPositional}:
          raise newMissingPositionalError(result, currentCommand, posId, position)
        else:
          # Fill in default positionals
          result.addElementFrom cmdline, posId
          let default = cmdline.valueDesc[posId].default()
          if default.isSome():
            result.value[posId] = @[default.get()]

  # Fill in default flags
  for flag in cmdline.flags(currentCommand):
    if flag notin result.value:
      result.addElementFrom cmdline, flag
      result.value[flag] = @[cmdline.valueDesc[flag].default().get()]

proc `[]`*[T: bool or int or string or float](parsed: ParseResult, flag: Flag[T]): T =
  when T is bool:
    parsed.value[Id flag][0].boolVal
  elif T is int:
    parsed.value[Id flag][0].intVal
  elif T is string:
    parsed.value[Id flag][0].strVal
  elif T is float:
    parsed.value[Id flag][0].floatVal

proc command*(parsed: ParseResult): Command =
  parsed.command[^1]
