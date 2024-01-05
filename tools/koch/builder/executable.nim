import std/options
import core

type
  ExeFlag* = enum
    ReleaseMode
    DangerMode
    EnableDebugSymbols

  ExeConfig* = object
    ## Executable configuration
    source*: BuildObject
    flags*: set[ExeFlag]
    extraFlags*: seq[string]
    cacheSuffix*: string

  Executable* = distinct BuildTarget

func addExecutable*(b: var Builder, targetName: string, config: ExeConfig): Executable =
  b.checkObject(config.source)
