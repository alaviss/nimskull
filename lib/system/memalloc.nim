type
  AllocLayout* {.requiresInit.} = object
    ## An allocation layout description
    size: Natural
    alignment: Positive

proc size*(layout: AllocLayout): int =
  ## Returns the minimum allocation size in bytes described by `layout`.
  layout.size

proc alignment*(layout: AllocLayout): int =
  ## Returns the mininum alignment in bytes described by `layout`.
  layout.alignment

proc allocLayout*(size: Natural, alignment: Positive): AllocLayout {.inline, raises: [], tags: [].}
  ## Returns an allocation layout from `size` and `alignment`, given that
  ## the following requirements are met:
  ##
  ## * `align` must be a power-of-two and not be zero.
  ## * `size` rounded up to the nearest multiple of `align` must be
  ##   smaller or equal to `high(Natural)`.
  ##
  ## A `Defect` will be raised if requirements are not met.

proc allocLayoutUnchecked*(size: Natural, alignment: Positive): AllocLayout {.inline.} =
  ## Returns an allocation layout from `size` and `alignment`, assuming that
  ## the requirements as specified by `allocLayout <#allocLayout,Natural,Natural>`_
  ## are met.
  AllocLayout(size: size, alignment: alignment)

proc layoutOf*(T: typedesc): AllocLayout {.inline.} =
  ## Returns the allocation layout of type `T`.
  AllocLayout(size: sizeof(T), alignment: alignof(T))

{.push rangeChecks: off.}
# By set constraints of `allocLayout`, we know the rounded up size will
# always be smaller or equal to `high(Natural)`, so skip checks on this
# (potentially) hot path.

proc padToAlignment*(layout: AllocLayout): AllocLayout {.inline.} =
  ## Returns the allocation layout with its minimum size rounded up to a
  ## multiple of its alignment.

  let
    alignmentMask = layout.alignment.uint - 1u
    paddedSize = Natural layout.size.uint + alignmentMask and not alignmentMask

  AllocLayout(size: paddedSize, alignment: layout.alignment)
{.pop.}

proc alignTo*(layout: AllocLayout, alignment: Positive): AllocLayout {.inline.} =
  ## Returns a new allocation layout from `layout` that is also aligned to
  ## `alignment`. The new layout is subjected to the requirements specified by
  ## `allocLayout <#allocLayout,Natural,Natural>`_.
  if layout.alignment < alignment:
    allocLayout(layout.size, alignment)
  else:
    layout

proc extend*(layout, next: AllocLayout): tuple[layout: AllocLayout, offset: int] {.inline.} =
  ## Returns the allocation layout for a memory block for storing a record
  ## of `layout` and `next` contiguously, where each element starts at an
  ## aligned address.
  ##
  ## The returned `offset` describes the distance between the start of the
  ## memory block to the beginning of the element described by `next`.
  let
    newAlignment = max(layout.alignment, next.alignment)
    padded = layout.alignTo(newAlignment).padToAlignment()

  (layout: allocLayout(padded.size + next.size, newAlignment),
   offset: padded.size)

proc repeat*(layout: AllocLayout, count: Positive): tuple[layout: AllocLayout, offset: int] {.inline.} =
  ## Returns the allocation layout for a memory block for storing `count`
  ## instances of `layout`, where each element starts at an aligned address.
  ##
  ## The returned `offset` describes the distance between the start of each
  ## element within an array.
  let padded = layout.padToAlignment()

  (layout: allocLayout(padded.size * count.Natural, layout.alignment),
   offset: padded.size)

when notJSnotNims:
  proc zeroMem*(p: pointer, size: Natural) {.inline, noSideEffect,
    tags: [], locks: 0, raises: [].}
    ## Overwrites the contents of the memory at `p` with the value 0.
    ##
    ## Exactly `size` bytes will be overwritten. Like any procedure
    ## dealing with raw memory this is **unsafe**.

  proc copyMem*(dest, source: pointer, size: Natural) {.inline, benign,
    tags: [], locks: 0, raises: [].}
    ## Copies the contents from the memory at `source` to the memory
    ## at `dest`.
    ## Exactly `size` bytes will be copied. The memory
    ## regions may not overlap. Like any procedure dealing with raw
    ## memory this is **unsafe**.

  proc moveMem*(dest, source: pointer, size: Natural) {.inline, benign,
    tags: [], locks: 0, raises: [].}
    ## Copies the contents from the memory at `source` to the memory
    ## at `dest`.
    ##
    ## Exactly `size` bytes will be copied. The memory
    ## regions may overlap, `moveMem` handles this case appropriately
    ## and is thus somewhat more safe than `copyMem`. Like any procedure
    ## dealing with raw memory this is still **unsafe**, though.

  proc equalMem*(a, b: pointer, size: Natural): bool {.inline, noSideEffect,
    tags: [], locks: 0, raises: [].}
    ## Compares the memory blocks `a` and `b`. `size` bytes will
    ## be compared.
    ##
    ## If the blocks are equal, `true` is returned, `false`
    ## otherwise. Like any procedure dealing with raw memory this is
    ## **unsafe**.

  proc cmpMem*(a, b: pointer, size: Natural): int {.inline, noSideEffect,
    tags: [], locks: 0, raises: [].}
    ## Compares the memory blocks `a` and `b`. `size` bytes will
    ## be compared.
    ##
    ## Returns:
    ## * a value less than zero, if `a < b`
    ## * a value greater than zero, if `a > b`
    ## * zero, if `a == b`
    ##
    ## Like any procedure dealing with raw memory this is
    ## **unsafe**.

when hasAlloc and not defined(js):

  proc allocImpl(layout: AllocLayout): pointer {.noconv, rtl, tags: [], benign, raises: [].}
  proc alloc0Impl(layout: AllocLayout): pointer {.noconv, rtl, tags: [], benign, raises: [].}
  proc deallocImpl(p: pointer, layout: AllocLayout) {.noconv, rtl, tags: [], benign, raises: [].}
  proc reallocImpl(p: pointer, oldLayout, newLayout: AllocLayout): pointer {.noconv, rtl, tags: [], benign, raises: [].}
  proc realloc0Impl(p: pointer, oldLayout, newLayout: AllocLayout): pointer {.noconv, rtl, tags: [], benign, raises: [].}

  # Allocator statistics for memory leak tests

  {.push stackTrace: off.}

  type AllocStats* = object
    allocCount: int
    deallocCount: int

  proc `-`*(a, b: AllocStats): AllocStats =
    result.allocCount = a.allocCount - b.allocCount
    result.deallocCount = a.deallocCount - b.deallocCount

  template dumpAllocstats*(code: untyped) =
    let stats1 = getAllocStats()
    code
    let stats2 = getAllocStats()
    mixin `$`
    echo $(stats2 - stats1)

  when defined(nimAllocStats):
    var stats: AllocStats
    template incStat(what: untyped) = inc stats.what
    proc getAllocStats*(): AllocStats = stats

  else:
    template incStat(what: untyped) = discard
    proc getAllocStats*(): AllocStats = discard

  template alloc*(layout: AllocLayout): pointer =
    ## Allocates a new memory block capable of storing data described by
    ## `layout`. `layout` **must** have a size larger than 0.
    ##
    ## The block has to be freed with
    ## `dealloc(block, layout) <#dealloc,pointer,AllocLayout>`_.
    ## The block is not initialized, so reading
    ## from it before writing to it is undefined behaviour!
    ##
    ## See also:
    ## * `alloc0 <#alloc0.t,AllocLayout>`_
    when defined(nimDebugAlloc):
      assert layout.size > 0, "Layout must be larger than 0."
    incStat(allocCount)
    allocImpl(layout)

  proc createU*(T: typedesc): ptr T {.inline, benign, raises: [].} =
    ## Allocates a new memory block capable of storing a value of type `T`.
    ##
    ## The block has to be freed with
    ## `dealloc(block, layoutOf(T)) <#dealloc.t,pointer,AllocLayout>`_.
    ## The block is not initialized, so reading
    ## from it before writing to it is undefined behaviour!
    ##
    ## See also:
    ## * `create <#create,typedesc>`_
    ## * `createArrayU <#createArrayU,typedesc,Positive>`_
    ## * `createArray <#createArray,typedesc,Positive>`_
    when T is UncheckedArray:
      {.error: "UncheckedArray[T] has no size. Use T instead".}
    cast[ptr T](alloc(layoutOf T))

  proc createArrayU*(T: typedesc, count: Positive): (ptr UncheckedArray[T], AllocLayout) {.inline, benign, raises: [].} =
    ## Allocates a new memory block capable of storing `count` elements
    ## of type `T` contiguously.
    ##
    ## The block has to be freed with
    ## `dealloc(block, layout) <#dealloc.t,pointer,AllocLayout>`_,
    ## where `layout` is the returned `AllocLayout`. The block is not
    ## initialized, so reading from it before writing to it is
    ## undefined behaviour!
    ##
    ## See also:
    ## * `createU <#createU,typedesc>`_
    ## * `createArray <#createArray,typedesc,Positive>`_
    when T is UncheckedArray:
      {.error: "UncheckedArray[T] has no size. Use T instead".}
    # We can skip offset, since all layouts from a type is pre-padded, so
    # offset is always equal to size.
    let (layout, _) = layoutOf(T).repeat(count)
    (cast[ptr UncheckedArray[T]](alloc(layout)), layout)

  template alloc0*(layout: AllocLayout): pointer =
    ## Allocates a new memory block capable of storing data described by
    ## `layout`. `layout` **must** have a size larger than 0.
    ##
    ## The block has to be freed with
    ## `dealloc(block, layout) <#dealloc,pointer,AllocLayout>`_.
    ## The block is initialized with all bytes containing zero, so it is
    ## somewhat safer than `alloc <#alloc.t,AllocLayout>`_.
    when defined(nimDebugAlloc):
      assert layout.size > 0, "Layout must be larger than 0."
    incStat(allocCount)
    alloc0Impl(layout)

  proc create*(T: typedesc): ptr T {.inline, benign, raises: [].} =
    ## Allocates a new memory block capable of storing a value of type `T`.
    ##
    ## The block has to be freed with
    ## `dealloc(block, layoutOf(T)) <#dealloc,pointer,AllocLayout>`_.
    ## The block is initialized with all bytes containing zero, so it is
    ## somewhat safer than `createU <#createU,typedesc>`_.
    when T is UncheckedArray:
      {.error: "UncheckedArray[T] has no size. Use T instead".}
    cast[ptr T](alloc0(layoutOf T))

  proc createArray*(T: typedesc, count: Positive): (ptr UncheckedArray[T], AllocLayout) {.inline, benign, raises: [].} =
    ## Allocates a new memory block capable of storing `count` elements
    ## of type `T` contiguously.
    ##
    ## The block has to be freed with
    ## `dealloc(block, layout) <#dealloc.t,pointer,AllocLayout>`_,
    ## where `layout` is the returned `AllocLayout`. The block is not
    ## initialized, so reading from it before writing to it is
    ## undefined behaviour!
    ##
    ## See also:
    ## * `create <#create,typedesc>`_
    ## * `createArrayU <#createArray,typedesc,Positive>`_
    when T is UncheckedArray:
      {.error: "UncheckedArray[T] has no size. Use T instead".}
    # We can skip offset, since all layouts from a type is pre-padded, so
    # offset is always equal to size.
    let (layout, _) = layoutOf(T).repeat(count)
    (cast[ptr T](alloc0(layout)), layout)

  template realloc*(p: sink pointer, oldLayout, newLayout: AllocLayout): pointer =
    ## Grows or shrinks a given memory block to be capable of storing data
    ## described by `newLayout`.
    ##
    ## `p` must be a pointer previously allocated via other standard library
    ## allocation procedures and cannot be `nil`. `p` is considered invalid
    ## after this call returns, even if the returned memory block is identical.
    ##
    ## The new layout **must** have a size larger than 0.
    ##
    ## Contents in the byte range
    ## `0 ..< min(oldLayout.size, newLayout.size)` are preserved from the old
    ## allocation. The byte range `oldLayout.size ..< newLayout.size` are not
    ## initialized, so reading from it before writing to it is undefined
    ## behaviour!
    ##
    ## The returned memory block must be freed with
    ## `dealloc(block, newLayout) <#dealloc.t,pointer,AllocLayout>`_.
    when defined(nimDebugAlloc):
      assert newLayout.size > 0, "New layout size must be larger than 0."
    reallocImpl(p, oldLayout, newLayout)

  template realloc0*(p: pointer, oldLayout, newLayout: AllocLayout): pointer =
    ## Grows or shrinks a given memory block to be capable of storing data
    ## described by `newLayout`.
    ##
    ## `p` must be a pointer previously allocated via other standard library
    ## allocation procedures and cannot be `nil`. `p` is considered invalid
    ## after this call returns, even if the returned memory block is identical.
    ##
    ## The new layout **must** have a size larger than 0.
    ##
    ## Contents in the byte range
    ## `0 ..< min(oldLayout.size, newLayout.size)` are preserved from the old
    ## allocation. The byte range `oldLayout.size ..< newLayout.size` are
    ## initialized with all bytes containing zero, so it is somewhat safer
    ## than `realloc <#realloc.t,pointer,AllocLayout,AllocLayout>`_.
    ##
    ## The returned memory block must be freed with
    ## `dealloc(block, newLayout) <#dealloc.t,pointer,AllocLayout>`_.
    when defined(nimDebugAlloc):
      assert newLayout.size > 0, "New layout size must be larger than 0."
    realloc0Impl(p, oldLayout, newLayout)

  proc resize*[T](p: ptr UncheckedArray[T], layout: AllocLayout, newCount: Positive): (ptr UncheckedArray[T], AllocLayout) {.inline, benign, raises: [].} =
    ## Grows or shrinks a given memory block to be capable of storing at least
    ## `newCount` elements of type `T`.
    ##
    ## `p` must be a pointer previously allocated via other standard library
    ## allocation procedures and cannot be `nil`. `p` is considered invalid
    ## after this call returns, even if the returned memory block is identical.
    ##
    ## Contents in the byte range
    ## `0 ..< min(layout.size, newLayout.size)` are preserved from the old
    ## allocation, where `newLayout` is the returned `AllocLayout`. The byte
    ## range `oldLayout.size ..< newLayout.size` are not initialized, so reading
    ## from it before writing to it is undefined behaviour!
    ##
    ## The returned memory block must be freed with
    ## `dealloc(block, newLayout) <#dealloc.t,pointer,AllocLayout>`_.
    when T is UncheckedArray:
      {.error: "UncheckedArray[T] has no size. Use T instead".}
    # We can skip offset, since all layouts from a type is pre-padded, so
    # offset is always equal to size.
    let (newLayout, _) = layoutOf(T).repeat(newCount)
    cast[ptr T](realloc(p, layout, newLayout))

  proc resize0*[T](p: ptr UncheckedArray[T], layout: AllocLayout, newCount: Positive): (ptr UncheckedArray[T], AllocLayout) {.inline, benign, raises: [].} =
    ## Grows or shrinks a given memory block to be capable of storing at least
    ## `newCount` elements of type `T`.
    ##
    ## `p` must be a pointer previously allocated via other standard library
    ## allocation procedures and cannot be `nil`. `p` is considered invalid
    ## after this call returns, even if the returned memory block is identical.
    ##
    ## Contents in the byte range
    ## `0 ..< min(layout.size, newLayout.size)` are preserved from the old
    ## allocation, where `newLayout` is the returned `AllocLayout`. The byte
    ## range `layout.size ..< newLayout.size` are initialized with all bytes
    ## containing zero, so it is somewhat safer than
    ## `resize0 <#resize[T],ptrUncheckedArrayT,AllocLayout,Positive>`_.
    ##
    ## The returned memory block must be freed with
    ## `dealloc(block, newLayout) <#dealloc.t,pointer,AllocLayout>`_.
    when T is UncheckedArray:
      {.error: "UncheckedArray[T] has no size. Use T instead".}
    # We can skip offset, since all layouts from a type is pre-padded, so
    # offset is always equal to size.
    let (newLayout, _) = layoutOf(T).repeat(newCount)
    cast[ptr T](realloc0(p, layout, newLayout))

  proc dealloc*(p: pointer, layout: AllocLayout) {.noconv, compilerproc, rtl, benign, raises: [], tags: [].} =
    ## Frees the memory allocated with `alloc`, `alloc0`,
    ## `realloc`, `realloc0`, `create`, `createU`, `createArray` or
    ## `createArrayU`. The given pointer must **not** be nil.
    ##
    ## **This procedure is dangerous!**
    ## If one forgets to free the memory a leak occurs; if one tries to
    ## access freed memory (or just freeing it twice!) a core dump may happen
    ## or other memory may be corrupted.
    ##
    ## The given `layout` **must** accurately describes the memory layout of `p`.
    incStat(deallocCount)
    deallocImpl(p, layout)

  {.pop.}

# GC interface:

when hasAlloc:
  proc getOccupiedMem*(): int {.rtl.}
    ## Returns the number of bytes that are owned by the process and hold data.

  proc getFreeMem*(): int {.rtl.}
    ## Returns the number of bytes that are owned by the process, but do not
    ## hold any meaningful data.

  proc getTotalMem*(): int {.rtl.}
    ## Returns the number of bytes that are owned by the process.


when defined(js):
  # Stubs:
  proc getOccupiedMem(): int = return -1
  proc getFreeMem(): int = return -1
  proc getTotalMem(): int = return -1

  proc dealloc(p: pointer, layout: AllocLayout) = discard
  proc alloc(layout: AllocLayout): pointer = discard
  proc alloc0(layout: AllocLayout): pointer = discard
  proc realloc(p: pointer, oldLayout, newLayout: AllocLayout): pointer = discard
  proc realloc0(p: pointer, oldLayout, newLayout: AllocLayout): pointer = discard

when hasAlloc and hasThreadSupport and not defined(useMalloc):
  proc getOccupiedSharedMem*(): int {.rtl.}
    ## Returns the number of bytes that are owned by the process
    ## on the shared heap and hold data. This is only available when
    ## threads are enabled.

  proc getFreeSharedMem*(): int {.rtl.}
    ## Returns the number of bytes that are owned by the
    ## process on the shared heap, but do not hold any meaningful data.
    ## This is only available when threads are enabled.

  proc getTotalSharedMem*(): int {.rtl.}
    ## Returns the number of bytes on the shared heap that are owned by the
    ## process. This is only available when threads are enabled.
