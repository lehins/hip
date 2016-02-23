{-# LANGUAGE BangPatterns #-}
module Graphics.Image.Processing.Convolution (
  Outside(..), convolve, convolve', convolveRows, convolveCols
  ) where

import Graphics.Image.Interface
import Graphics.Image.Processing.Geometric


-- | Approach to be used near the border during convolution.
data Outside px =
  Extend      -- ^ Extend the input image by repeating the border rows and
              -- columns.
  | Wrap      -- ^ Wrap the input image (thereby making it periodic) by assxuming
              -- the first column comes immediately after the last, etc.
  | Fill !px  -- ^ Fill in a constant pixel for output pixels too near the
              -- border.
  | Crop      -- ^ Compute an image of redused size, by eliminating output rows
              -- and columns that cannot be computed by convolution.



convolve'' :: Array arr cs e =>
              Outside (Pixel cs e) -> Image arr cs e -> Image arr cs e -> Image arr cs e
convolve'' !out !kernel !img = traverse2 kernel img (getDims out) stencil where
  !(krnM, krnN)     = dims kernel        
  !(imgM, imgN)     = dims img
  !krnM2@borderUp   = krnM `div` 2
  !krnN2@borderLeft = krnN `div` 2
  !borderDown       = imgM - krnM2 - 1
  !borderRight      = imgN - krnN2 - 1
  getDims Crop _ _ = (imgM - krnM, imgN - krnN)
  getDims _    _ _ = (imgM, imgN)
  {-# INLINE getDims #-}
  getOutFunc Extend    = outExtend
  getOutFunc Wrap      = outWrap
  getOutFunc (Fill px) = outFill px
  getOutFunc Crop      = outCrop
  {-# INLINE getOutFunc #-}
  outExtend !getPx !i !j = getPx (if i < 0 then 0 else if i >= imgM then imgM - 1 else i,
                                  if j < 0 then 0 else if j >= imgN then imgN - 1 else j)
  {-# INLINE outExtend #-}
  outWrap !getPx !i !j   = getPx (i `mod` imgM, j `mod` imgN)
  {-# INLINE outWrap #-}
  outFill !px _ _ _      = px
  {-# INLINE outFill #-}
  outCrop !getPx !i !j   = getPx (i + krnM2, j + krnN2)
  {-# INLINE outCrop #-}
  stencil !getKrnPx !getImgPx !(i, j) = integrate 0 0 0 where
    getImgPx' !i' !j' | j' < borderLeft  ||
                        j' > borderRight ||
                        i' < borderUp    ||  
                        i' > borderDown = getOutFunc out getImgPx i' j'
                      | otherwise       = getImgPx (i', j')
    {-# INLINE getImgPx' #-}
    !ikrnM = i - krnM2
    !jkrnN = j - krnN2
    integrate !ki !kj !acc
      | kj == krnN            = integrate (ki+1) 0 acc
      | kj == 0 && ki == krnM = acc
      | otherwise             = let !krnPx = getKrnPx (ki, kj)
                                    !imgPx = getImgPx' (ki + ikrnM) (kj + jkrnN)
                                in integrate ki (kj + 1) (acc + krnPx * imgPx)
    {-# INLINE integrate #-}
  {-# INLINE stencil #-}
{-# INLINE convolve'' #-}


convolve  :: (Transformable arr' arr, Array arr' cs e, ManifestArray arr cs e) =>
             Outside (Pixel cs e)   -- ^ Approach to be used near the borders.
          -> Image arr' cs e -- ^ Convolution kernel image.
          -> Image arr cs e -- ^ Image that will be convolved with the kernel.
          -> Image arr cs e
convolve !out = convolve'' out . transform (undefined :: arr) . rotate180
{-# INLINE convolve #-}


convolve'  :: Array arr cs e =>
              Outside (Pixel cs e)   -- ^ Approach to be used near the borders.
           -> Image arr cs e -- ^ Convolution kernel image.
           -> Image arr cs e -- ^ Image that will be convolved with the kernel.
           -> Image arr cs e
convolve' !out = convolve'' out . rotate180
{-# INLINE convolve' #-}


convolveRows :: ManifestArray arr cs e =>
                Outside (Pixel cs e) -> [Pixel cs e] -> Image arr cs e -> Image arr cs e
convolveRows !out = convolve' out . fromLists . (:[]) . reverse
{-# INLINE convolveRows #-}


convolveCols :: ManifestArray arr cs e =>
                Outside (Pixel cs e) -> [Pixel cs e] -> Image arr cs e -> Image arr cs e
convolveCols !out = convolve' out . transpose . fromLists . (:[]) . reverse
{-# INLINE convolveCols #-}

