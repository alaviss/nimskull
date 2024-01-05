import strformat

type
  BuildGraph* = object
    targetName: seq[string]
    targetKind: seq[BuildTargetKind]

    buildExeTarget: ExeBuilder

  BuildTarget* = distinct uint16

  TargetConfig* = object
    name*: string
    kind*: BuildTargetKind

  BuildTargetKind* = enum
    Noop
    BuildExe

func name*(b: BuildGraph, t: BuildTarget): string =
  b.targetName[t.int]

func outputPath*(b: BuildGraph, t: BuildTarget): string =
  case b.targetKind[t.int]
  else: ""

func nextTarget*(b: BuildGraph): BuildTarget =
  b.targetName.len()

func addTarget*(b: var BuildGraph, id: BuildTarget, conf: TargetConfig) =
  doAssert b.nextTarget() == id
  b.targetName.add(conf.name)
  b.targetKind.add(conf.kind)
