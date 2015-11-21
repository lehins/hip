module HIP.Processing (
  flipH, flipV,
  module HIP.Processing.Convolution,
  module HIP.Processing.Geometric
  ) where

import HIP.Interface
import HIP.Processing.Geometric
import HIP.Processing.Convolution


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
  
