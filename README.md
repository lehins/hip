Haskell Image Processing Library
================================

Haskell Image Processing (HIP) Library

Documentation is on [Hackage](http://hackage.haskell.org/package/hip).

Installation
------------

HIP depends on `Chart-cairo` package for creating histograms, which has some dependencies:

* `$ sudo apt-get install libcairo2-dev`

and depending what you prefer `cabal` or `stack`:

* `$ cabal update && cabal install gtk2hs-buildtool`
* `$ stack install gtk2hs-buildtools-0.13.2.1`

Install HIP using `cabal`:

* `$ cabal update && cabal install hip`

and using `stack` (from source code only, not yet on stackage):
* `$ stack install`


In order to be able to view images in GHCi and external program of your choice
is used. On Linux I recommend `GPicView`, but you can use any viewer that
accepts a filename as an argument.

