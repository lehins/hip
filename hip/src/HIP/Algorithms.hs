module HIP.Algorithms (
  flipH, flipV,
  module HIP.Algorithms.Convolution,
  module HIP.Algorithms.Fourier,
  module HIP.Algorithms.Geometric,
  module HIP.Algorithms.Interpolation
  ) where

import HIP.Interface
import HIP.Algorithms.Convolution
import HIP.Algorithms.Fourier
import HIP.Algorithms.Geometric
import HIP.Algorithms.Interpolation

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
  
