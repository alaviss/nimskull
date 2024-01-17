import std/[packedsets, tables]

type
  BuildGraph* = object
    targetName: seq[string]
    targetKind: seq[BuildTargetKind]
    targetRequires: Table[BuildTarget, PackedSet[BuildTarget]]
    targetFlags: Table[BuildTarget, seq[string]]

    exeGraph: ExeGraph

  ExeGraph* = object
    targetSettings: Table[BuildTarget, set[Settings]]
    targetSource: Table[BuildTarget, string]
    compileFlags: seq[string]
    compiler: string
    settings: set[Settings]

  BuildTarget* = distinct uint16
  ExeTarget* = distinct BuildTarget
  RunTarget* = distinct BuildTarget

  TargetConfig* = object
    name*: string
    kind*: BuildTargetKind
    requires*: PackedSet[BuildTarget]
    flags*: seq[string]

  ExeConfig* = object
    name*: string
    source*: string
    requires*: PackedSet[BuildTarget]
    flags*: seq[string]
    settings*: set[ExeSetting]

  RunConfig* = object
    name*: string
    exe*: string
    requires*: PackedSet[BuildTarget]
    flags*: seq[string]

  BuildTargetKind* = enum
    Noop
    BuildExe
    RunExe

  ExeSetting* = enum
    ReleaseMode
    DangerMode
    Debuginfo

func name*(b: BuildGraph, t: BuildTarget): string =
  b.targetName[t.int]

func outputPath*(b: BuildGraph, t: BuildTarget): string =
  case b.targetKind[t.int]
  else: ""

func flags*(b: BuildGraph, t: BuildTarget): seq[string] =
  b.targetFlags.getOrDefault(t)

func kind*(b: BuildGraph, t: BuildTarget): BuildTargetKind =
  b.targetKind[t]

func requires*(b: BuildGraph, t: BuildTarget, recursive = false): PackedSet[BuildTarget] =
  result = b.targetRequires.getOrDefault(t)
  if recursive:
    let direct = result
    for t in direct.items():
      result.incl b.requires(t, recursive = true)

func nextTarget*(b: BuildGraph): BuildTarget =
  b.targetName.len()

func addTarget*(b: var BuildGraph, id: BuildTarget, conf: TargetConfig) =
  doAssert b.nextTarget() == id
  b.targetName.add(conf.name)
  b.targetKind.add(conf.kind)
  if not conf.requires.isNil():
    b.targetRequires[id] = conf.requires
  if conf.flags.len() > 0:
    b.targetFlags[id] = conf.flags

func addExecutable*(b: var BuildGraph, conf: ExeConfig): ExeTarget =
  let id = b.nextTarget()
  if conf.settings.len > 0:
    b.exeGraph.targetSettings[id] = conf.settings

  b.exeGraph.targetSource[id] = conf.source

  b.addTarget(id, TargetConfig(
    name: conf.name,
    kind: BuildExe,
    requires: conf.requires,
    flags: conf.flags
  ))

  ExeTarget(id)

func addRun*(b: var BuildGraph, conf: RunConfig): RunTarget =
  let id = b.nextTarget()
  let flags = newSeqOfCap[string](conf.flags.len + 1)
  flags.add conf.exe
  flags.add conf.flags
  b.addTarget(id, TargetConfig(
    name: conf.name,
    kind: RunExe,
    requires: conf.requires,
    flags: flags
  ))

  RunTarget(id)
