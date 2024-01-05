import core

type
  RunConfig* = object
    exe*: BuildObject
    args*: seq[BuildObject]

  RunTarget* = distinct BuildTarget

func addRun*(b: var Builder, name: string, config: RunConfig): RunTarget
