Haskell Image Processing Library
================================

Haskell Image Processing (HIP) Library

**Warning** - Currently HIP is undergoing a major rewrite with
[massiv](http://hackage.haskell.org/package/massiv) therefore `master` branch is not
stable at the moment. For currently released version see
[`1.x`](https://github.com/lehins/hip/tree/hip-1.x) branch.

Documentation is on [Hackage](http://hackage.haskell.org/package/hip), and [Stackage](https://www.stackage.org/package/hip).

[![Build Status](https://travis-ci.org/lehins/hip.svg?branch=master)](https://travis-ci.org/lehins/hip)
[![Hackage](https://img.shields.io/hackage/v/hip.svg?style=flat)](https://hackage.haskell.org/package/hip)

Installation
------------

Dependencies:

```
$ sudo apt-get install zlib1g-dev
```


Install HIP using `cabal`:

* `$ cabal update && cabal install hip`

and using `stack`:

* `$ stack install hip`

In order to be able to view images in GHCi an external image viewer is used. You
can use any viewer that accepts a filename as an argument, and by default, image
viewer specified by the OS is used.

