{.push stackTrace: off.}
proc allocImpl(layout: AllocLayout): pointer =
  # ISO C specifies that `malloc` returned memory block can store any data
  # with "fundamental alignment", of which c_max_align_t is the largest value.
  if layout.alignment <= alignof(c_max_align_t):
    result = c_malloc(layout.size.csize_t)
  else:
    result = when defined(windows):
      c_aligned_malloc(layout.alignment.csize_t, layout.size.csize_t)
    else:
      c_aligned_alloc(layout.alignment.csize_t, layout.size.csize_t)

  if result == nil:
    raiseOutOfMem()

proc alloc0Impl(layout: AllocLayout): pointer =
  if layout.alignment <= alignof(c_max_align_t):
    result = c_calloc(layout.size.csize_t, 1)
  else:
    result = when defined(windows):
      c_aligned_malloc(layout.alignment.csize_t, layout.size.csize_t)
    else:
      c_aligned_alloc(layout.alignment.csize_t, layout.size.csize_t)

    if result != nil:
      zeroMem(result, layout.size.csize_t)

  if result == nil:
    raiseOutOfMem()

proc reallocImpl(p: pointer, oldLayout, newLayout: AllocLayout): pointer =
  if oldLayout.alignment <= alignof(c_max_align_t) and newLayout.alignment <= alignment(c_max_align_t):
    result = c_realloc(p, newLayout.size.csize_t)
  else:
    result = alloc(newLayout)
    copyMem(result, p, min(oldLayout.size, newLayout.size))
    dealloc(p, oldLayout)

proc realloc0Impl(p: pointer, oldLayout, newLayout: AllocLayout): pointer =
  result = realloc(p, oldLayout, newLayout)
  if newLayout.size > oldLayout.size:
    zeroMem(cast[pointer](cast[int](result) + oldLayout.size), newLayout.size - oldLayout.size)

proc deallocImpl(p: pointer, layout: AllocLayout) =
  when defined(windows):
    if layout.alignment > alignof(c_max_align_t):
      c_aligned_free(p)
      return

  c_free(p)

# Empty stubs for the GC
proc getOccupiedMem(): int = discard
proc getFreeMem(): int = discard
proc getTotalMem(): int = discard
{.pop.}
