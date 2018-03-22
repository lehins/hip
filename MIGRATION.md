# Migration notes from 1.x => 2.x

## Major changes

* No more representations.
* Only computation backend is `massiv`
* All of the images are backed by pinned memory.
* Automatic fusion.
* Tuples are no longer used for indexing

## Specifics

* No more `Array` or `MArray` type class, only restriction that is left on all functions is
  `ColorSpace`.
* `traverse` is somewhat different
