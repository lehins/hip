1.5.4.0
=======

* Addition of `disable-chart` flag
* Bunch of semi-functional stuff from GSoC 2018

1.5.2.0
=======

* Fixed FFT performace issue
* Reduced JuicyPixels module compile time and introduced conversion functions.
* Created `Filter` and implemented few filters: `gaussianBlur`
* Indroduced `Seq` format wrapper for sequence of images (eg. animated GIFs)

1.5.2.0
=======

* Fixed `Storable` instance for some Pixel types.
* Fixed reading/writing animated GIFs. Added a detailed example to documentation.
* Improved encoding/decoding of images.
* Improved coversion between `ColorSpace`s.

1.5.1.0
=======

* Significantly improved convolution performance.
* Improved exchanging representation between images, by removing `Exchangable`
  class, but keeping `exchange` function usage unchaged, thus it should be
  backwards compatible up to a type signature.
* Created general `fromVector` to `toVector` functions
* Brought back `fromRepaArrayS` and `fromRepaArrayP` functions [#6]

1.5.0.0
=======

* Refactored `Gray` color space to be `X`, in order to reflect it's generality
* Renamed few core functions:

  * `mapPx` -> `liftPx`,
  * `zipWithPx` -> `liftPx2`,
  * `broadcastC` -> `promote`,
  * `singleton` -> `scalar`.

* `upsample`/`downsample` functions are now a lot more general.


1.4.0.1
=======

* Fixed the ability to construct complex images by installing `Complex` into `Elevator`
* Made it possible to write complex images by concatenating real and imaginary part together.
* Fixed writing images in other representation than `VS`.

1.4.0.0
=======

Major rewrite, with most of functionality is still backwards compatible, but
with a some extra features.

* Storable Repa and Vector representations through generic implementations
* `ColorSpace` is more general allowing for non polymorphic Pixel types
* `Elevator` works on base types rather than on pixels
* Conversions from JuicyPixels and NetPbm is done through casting a Vector
  rather than through an explicit conversion.

Major API changes:

  * Renaming `RS` and `RP` Repa representations into `RSU` and `RPU`.
  * Addition `VS` Storable Vector representationas well as `RSS` and `RPS`
    Storable Repa representations.

1.3.0.0
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
