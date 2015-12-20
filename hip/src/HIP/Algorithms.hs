{-# LANGUAGE BangPatterns, ViewPatterns #-}
module HIP.Algorithms (
  flipH, flipV, module X
  ) where

import HIP.Interface
import HIP.Algorithms.Convolution as X
import HIP.Algorithms.Fourier as X
import HIP.Algorithms.Geometric as X
import HIP.Algorithms.Interpolation as X

flipV :: AImage img px => img px -> img px
flipV !img@(dims -> (m, n)) = backpermute m n getNewIndex img where
  getNewIndex !i !j = (m - 1 - i, j)
  {-# INLINE getNewIndex #-}
{-# INLINE flipV #-}


flipH :: AImage img px => img px -> img px
flipH !img@(dims -> (m, n)) = backpermute m n getNewIndex img where
  getNewIndex !i !j = (i, n - 1 - j)
  {-# INLINE getNewIndex #-}
{-# INLINE flipH #-}
  
