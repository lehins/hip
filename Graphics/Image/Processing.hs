{-# LANGUAGE BangPatterns, FlexibleContexts, ViewPatterns #-}
module Graphics.Image.Processing (
  --module Graphics.Image.Processing.Convolution,
  --module Graphics.Image.Processing.FFT,
  module Graphics.Image.Processing.Geometric,
  downsampleRows, downsampleCols, downsample, downsampleF,
  upsampleRows, upsampleCols, upsample, upsampleF,
  leftToRight, topToBottom, pad
  ) where

import Graphics.Image.Interface
--import Graphics.Image.Processing.FFT
--import Graphics.Image.Processing.Matrix
import Graphics.Image.Processing.Geometric

downsampleF :: Image img px => Int -> Int -> img px -> img px
{-# INLINE downsampleF #-}
downsampleF !fm !fn !img = traverse img
                           (\m n -> (m `div` fm, n `div` fn))
                           (\getPx i j -> getPx (i*fm) (j*fn))


upsampleF :: (Image img px, Ord (Inner px), Num (Inner px)) => Int -> Int -> img px -> img px
{-# INLINE upsampleF #-}
upsampleF !fm !fn !img = traverse img 
                         (\m n -> (m*fm, n*fn))
                         (\getPx i j ->
                           if i `mod` fm == 0 && j `mod` fn == 0
                           then getPx (i `div` fm) (j `div` fn)
                           else pixel 0)

-- | Removes every second row from the image starting with second one.
downsampleRows :: (Image img px, Ord (Inner px), Num (Inner px)) => img px -> img px
{-# INLINE downsampleRows #-}
downsampleRows = downsampleF 2 1

downsampleCols :: (Image img px, Ord (Inner px), Num (Inner px)) => img px -> img px
{-# INLINE downsampleCols #-}
downsampleCols = downsampleF 1 2

downsample :: (Image img px, Ord (Inner px), Num (Inner px)) => img px -> img px
{-# INLINE downsample #-}
downsample = downsampleF 2 2

upsampleRows :: (Image img px, Ord (Inner px), Num (Inner px)) => img px -> img px
{-# INLINE upsampleRows #-}
upsampleRows = upsampleF 2 1

upsampleCols :: (Image img px, Ord (Inner px), Num (Inner px)) => img px -> img px
{-# INLINE upsampleCols #-}
upsampleCols = upsampleF 1 2

upsample :: (Image img px, Ord (Inner px), Num (Inner px)) => img px -> img px
{-# INLINE upsample #-}
upsample = upsampleF 2 2

{- | Concatenates two images together into one. Both input images must have the
same number of rows. -}
leftToRight :: Image img px => img px -> img px -> img px
{-# INLINE leftToRight #-}
leftToRight !img1@(cols -> !n1) !img2 = traverse2 img1 img2 newDims newPx where
  newDims !m1 _ !m2 !n2
    | m1 == m2  = (m1, n1 + n2)
    | otherwise = error ("Images must agree in numer of rows, but received: "++
                         (show img1)++" and "++(show img2))
  {-# INLINE newDims #-}
  newPx !getPx1 !getPx2 !i !j = if j < n1 then getPx1 i j else getPx2 i (j-n1)
  {-# INLINE newPx #-}

{- | Concatenates two images together into one. Both input images must have the
same number of columns. -}
topToBottom :: Image img px => img px -> img px -> img px
{-# INLINE topToBottom #-}
topToBottom !img1@(rows -> !m1) !img2 = traverse2 img1 img2 newDims newPx where
  newDims _ n1 !m2 !n2
    | n1 == n2  = (m1 + m2, n1)
    | otherwise = error ("Images must agree in numer of columns, but received: "++
                         (show img1)++" and "++(show img2))
  {-# INLINE newDims #-}
  newPx !getPx1 !getPx2 !i !j = if i < m1 then getPx1 i j else getPx2 (i-m1) j
  {-# INLINE newPx #-}

{- | Changes dimensions of an image while padding it with a default pixel whenever
@i@ and @j@ is out of bounds for an original image -}
pad :: Image img px =>
       px         -- ^ default pixel to be used for out of bounds region
    -> Int -> Int -- ^ image's new dimensions @m@ rows and @n@ columns
    -> img px     -- ^ image that is subjected to padding.
    -> img px
pad !defPx !m1 !n1 !img@(dims -> !(m, n)) =
  traverse img (const . const (m1, n1)) newPx where
    newPx !getPx !i !j = if i < m && j < n then getPx i j else defPx
    {-# INLINE newPx #-}
{-# INLINE pad #-}
  
