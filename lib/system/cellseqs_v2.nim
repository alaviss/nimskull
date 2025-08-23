#
#
#            Nim's Runtime Library
#        (c) Copyright 2019 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Cell seqs for cyclebreaker and orc.

type
  CellTuple[T] = (T, PNimTypeV2)
  CellArray[T] = ptr UncheckedArray[CellTuple[T]]
  CellSeq[T] = object
    len, cap: int
    d: CellArray[T]

proc add[T](s: var CellSeq[T], c: T; t: PNimTypeV2) {.inline.} =
  if s.len >= s.cap:
    let
      newCap = s.cap * 3 div 2
      oldLayout = layoutOf(CellTuple[T]).repeat(s.cap).layout
      newLayout = layoutOf(CellTuple[T]).repeat(newCap).layout
    s.d = cast[CellArray[T]](realloc(s.d, oldLayout, newLayout))
    s.cap = newCap

  s.d[s.len] = (c, t)
  inc(s.len)

proc init[T](s: var CellSeq[T], cap: int = 1024) =
  s.len = 0
  s.cap = cap
  let layout = layoutOf(CellTuple[T]).repeat(s.cap).layout
  s.d = cast[CellArray[T]](alloc(layout))

proc deinit[T](s: var CellSeq[T]) =
  if s.d != nil:
    let layout = layoutOf(CellTuple[T]).repeat(s.cap).layout
    dealloc(s.d, layout)
    s.d = nil
  s.len = 0
  s.cap = 0

proc pop[T](s: var CellSeq[T]): (T, PNimTypeV2) =
  result = s.d[s.len-1]
  dec s.len
