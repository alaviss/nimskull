type
  Builder* = object
    targetName: seq[string]

  BuildTarget* = distinct uint32

  BuildObject* = object
    case kind*: BuildObjectKind
    of String:
      str*: string
    of BuildTarget:
      target*: BuildTarget

  BuildObjectKind* = enum
    String
    BuildTarget

  BuildTargetKind = enum
    Special
    Executable
    Run

func target*(t: BuildTarget): BuildObject =
  BuildObject(kind: BuildTarget, target: t)

func string*(s: string): BuildObject =
  BuildObject(kind: String, str: s)

func makeTarget(kind: BuildTargetKind, id: uint32): BuildTarget =
  assert id and 0xf0000000 == 0, "id is too large"
  BuildTarget(kind.uint32 shr 28 or id)

func kind(b: BuildTarget): BuildTargetKind =
  BuildTargetKind((b.uint32 and 0xf0000000) shl 28)

func nameOf*(b: Builder, t: BuildTarget): string
func outputPath*(b: Builder, t: BuildTarget): string
