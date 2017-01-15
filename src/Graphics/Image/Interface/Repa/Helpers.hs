{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.Repa.Helpers where

import Data.Array.Repa
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Undefined

-- | Make a 2D windowed array from two others, one to produce the elements in
--   the internal region, and one to produce elements in the border region. The
--   two arrays must have the same extent.
--
makeWindowed
        :: (Source r1 a, Source r2 a)
        => DIM2                 -- ^ Extent of array.
        -> ((Int, Int), (Int, Int))  -- ^ Window points.
        -> Array r1 DIM2 a      -- ^ Array for internal elements.
        -> Array r2 DIM2 a      -- ^ Array for border elements.
        -> Array (P r1 (P r2 (P r2 (P r2 (P r2 X))))) DIM2 a
makeWindowed sh@(_ :. m :. n) !((it, jt), (ib, jb)) arrWindow arrBorder =
  checkDims `seq`
  let inInternal (Z :. i :. j) = i >= it && i < ib && j >= jt && j < jb
      {-# INLINE inInternal #-}
      inBorder = not . inInternal
      {-# INLINE inBorder #-}
  in APart sh (Range (Z :. it :. jt) (Z :. (ib - it) :. (jb - jt)) inInternal) arrWindow $
     APart sh (Range (Z :. 0 :.  0 ) (Z :. it        :. n        ) inBorder  ) arrBorder $
     APart sh (Range (Z :. it :. 0 ) (Z :. (ib - it) :. jt       ) inBorder  ) arrBorder $
     APart sh (Range (Z :. it :. jb) (Z :. (ib - it) :. (n - jb) ) inBorder  ) arrBorder $
     APart sh (Range (Z :. ib :. 0 ) (Z :. (m - ib)  :. n        ) inBorder  ) arrBorder $
     AUndefined sh
  where
    checkDims =
      if extent arrWindow == extent arrBorder
        then ()
        else error
               "makeWindowed: internal and border arrays have different extents"
    {-# NOINLINE checkDims #-}
{-# INLINE makeWindowed #-}
