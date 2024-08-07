discard """
description: "Tests for the cmdline module"
"""

import experimental/cmdline

block typicalUsage:
  ## Samples of typical command lines

  block catLike:
    ## Unix's cat clone

    var cl: Cmdline
    let opts = (
      showNonPrinting: cl.addFlag("show-nonprinting", "visualize nonprinting characters", false),
      squeezeBlank: cl.addFlag("squeeze-blank", "suppress repeated empty lines", false)
    )

    block dashdash:
      let parsed = cl.parse ["cat", "--show-nonprinting", "--", "--squeeze-blank"]
      doAssert parsed[opts.showNonPrinting]
      doAssert not parsed[opts.squeezeBlank]
      doAssert parsed.remaining == ["--squeeze-blank"]

    block basic:
      let parsed = cl.parse ["cat", "--show-nonprinting", "--squeeze-blank", "-"]
      doAssert parsed[opts.showNonPrinting]
      doAssert parsed[opts.squeezeBlank]
      doAssert parsed.remaining == ["-"]

    block bool:
      let parsed = cl.parse ["cat", "--show-nonprinting=no", "-"]
      doAssert not parsed[opts.showNonPrinting]
      doAssert not parsed[opts.squeezeBlank]
      doAssert parsed.remaining == ["-"]

  block seqLike:
    ## Unix's seq clone
    var cl: Cmdline
    let opts = (
      format: cl.addFlag("format", "use printf style floating-point format", ""),
      separator: cl.addFlag("separator", "use string to separate numbers", "\\n"),
      equalWidth: cl.addFlag("equal-width", "equalize width by padding with leading zeros", false)
    )

    block basic:
      let parsed = cl.parse ["seq", "10", "--format:%02d"]
      doAssert parsed[opts.format] == "%02d"
      doAssert parsed[opts.separator] == "\\n"
      doAssert parsed.remaining == ["10"]

    block separated:
      let parsed = cl.parse ["seq", "10", "--format", "--separator=;"]
      doAssert parsed[opts.format] == "--separator=;"
      doAssert parsed[opts.separator] == "\\n"
      doAssert parsed.remaining == ["10"]

    block multiple:
      let parsed = cl.parse ["seq", "10", "--separator", ";", "100"]
      doAssert parsed[opts.format] == ""
      doAssert parsed[opts.separator] == ";"
      doAssert parsed.remaining == ["10", "100"]

block flags:
  ## Tests for flags
  block:
    ## It is not possible to add duplicated flags
    var cl: Cmdline

    discard cl.addFlag("flag", "", false)
    doAssertRaises(ValueError):
      discard cl.addFlag("flag", "", false)

  block:
    ## `--` blocks stops all flag processing
    var cl: Cmdline

    let flag = cl.addFlag("flag", "", false)
    let parsed = cl.parse ["", "--", "--flag"]
    doAssert not parsed[flag]
    doAssert parsed.remaining == ["--flag"]

  block:
    ## Flag behavorial tests
    var cl: Cmdline
    let opts = (
      int: cl.addFlag("int", "int flag", 0),
      float: cl.addFlag("f", "float flag", 42.0),
      string: cl.addFlag("string", "string flag", "default"),
      bool: cl.addFlag("bool", "bool flag", false)
    )

    block simple:
      let parsed = cl.parse ["", "--f", "0", "--int", "42"]
      doAssert parsed[opts.int] == 42
      doAssert parsed[opts.float] == 0.0
      doAssert parsed[opts.string] == "default"
      doAssert not parsed[opts.bool]

    block replace:
      let parsed = cl.parse ["", "--int", "42", "--int=100"]
      doAssert parsed[opts.int] == 100

    block boolSpace:
      ## Boolean does not take non-delimited value
      let parsed = cl.parse ["", "--bool", "false"]
      doAssert parsed[opts.bool]
      doAssert parsed.remaining == ["false"]

    block boolDelim:
      ## Boolean only take delimited values
      let parsed = cl.parse ["", "--bool", "--bool:false"]
      doAssert not parsed[opts.bool]

    block help:
      ## Help triggers HelpError when unset
      doAssertRaises(HelpError):
        discard cl.parse ["", "--help"]

    block unknown:
      ## Unregistered flags will trigger an error
      try:
        discard cl.parse ["", "--unknown"]
        doAssert false, "expected UnknownFlagError"
      except UnknownFlagError as e:
        doAssert e.flagName == "--unknown"

    block novalue:
      ## Missing value will trigger an error
      try:
        discard cl.parse ["", "--int"]
        doAssert false, "expected MissingValueError"
      except MissingValueError as e:
        doAssert e.flagName == "--int"
        doAssert e.flagId == SomeFlag(opts.int)

    block wrongvalue:
      ## Invalid value also errors
      try:
        discard cl.parse ["", "--int=notint"]
        doAssert false, "expected InvalidValueError"
      except InvalidValueError as e:
        doAssert e.parent of ValueError
        doAssert e.flagName == "--int"
        doAssert e.flagId == SomeFlag(opts.int)
        doAssert e.flagValue == "notint"
