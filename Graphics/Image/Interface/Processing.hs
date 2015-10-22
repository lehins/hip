module Graphics.Image.Interface.Processing (
  flipH, flipV,
  module Graphics.Image.Interface.Processing.Convolution,
  module Graphics.Image.Interface.Processing.Geometric
  ) where

import Graphics.Image.Interface
import Graphics.Image.Interface.Processing.Geometric
import Graphics.Image.Interface.Processing.Convolution


flipH' :: AImage img px => img px -> img px
flipH' img@(dims -> (m, n)) = backpermute m n (flip (,)) img

flipV :: AImage img px => img px -> img px
flipV !img@(dims -> (m, n)) = backpermute m n getNewIndex img where
  getNewIndex !i !j = (m - i, j)
  {-# INLINE getNewIndex #-}
{-# INLINE flipV #-}


flipH :: AImage img px => img px -> img px
flipH !img@(dims -> (m, n)) = backpermute m n getNewIndex img where
  getNewIndex !i !j = (i, n - j)
  {-# INLINE getNewIndex #-}
{-# INLINE flipH #-}
  
