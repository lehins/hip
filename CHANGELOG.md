1.2.1.0
=======

* Significantly simplified interface:
  * Removed `ManifestArray`, `SequentialArray` and `MutableArray` classes in favor of `MArray`.
* Rewrite of Repa image representations. Removed `RD` representation.
* Introduced function `canvasSize`.

1.2.0.0
=======

* Changed `Interpolation` in a way that border resolution is supplied separatly
  from the method.
* Introduced function `translate`.
* Added a better test suite. Improved coverage.
* Fixed a bug with border resolution strategy `Continue`.
* Added better error messaging for border checks.

1.1.0.1
=======

* Backwards compatibility with GHC 7.8

1.1.0.0
=======

* GHC 8 support.
* Improved IO:
  * OS default image viewer is used for displaying images with ability to use a custom one.
  * Histogram plotting is done using diagrams instead of cairo backend,
    significantly simplifying installation
    

1.0.2.0
=======

* Changed the way image displaying works. Now `displayImage` function will try
  to automatically detect the default external viewer program.
* Renamed module `Graphics.Image.IO.External` to `Graphics.Image.IO.Formats`,
  so it reflects the purpose slightly better.

1.0.1.2
=======

* Added support of "vector>=0.11.0.0"

1.0.1.1
=======

* Added `rotate` function.
* Fixed writing `RGBA` files with `writeImage` (#2).

1.0.1
=====

* Made it compatible with GHC >= 7.4 (#1)
* Added histogram plotting using Charts
