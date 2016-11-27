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
* Made Histogram plotting using Chart dependency optional, which is controlled
  by a compile time flag `use-chart`.
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
