{-# LANGUAGE BangPatterns #-}
module HIP.Algorithms.Convolution (
  Outside(..), convolve,
  convolveRows, convolveCols
  ) where

import HIP.Interface

{- | Approach to be used near the border during convolution. -}
data Outside px =
  Extend      -- ^ Extend the input image by repeating the border rows and
              -- columns.
  | Wrap      -- ^ Wrap the input image (thereby making it periodic) by assxuming
              -- the first column comes immediately after the last, etc.
  | Fill !px  -- ^ Fill in a constant pixel for output pixels too near the
              -- border.
  | Crop      -- ^ Compute an image of redused size, by eliminating output rows
              -- and columns that cannot be computed by convolution.



convolve  :: AImage img px =>
              Outside px   -- ^ Approach to be used near the borders.
           -> img px       -- ^ Convolution kernel image.
           -> img px       -- ^ Image that will be used to convolve the kernel.
           -> img px
convolve !out !kernel !img = traverse2 kernel img (getDims out) stencil where
  !(krnM, krnN)     = dims kernel        
  !(imgM, imgN)     = dims img
  !krnM2@borderUp   = krnM `div` 2
  !krnN2@borderLeft = krnN `div` 2
  !borderDown       = imgM - krnM2 - 1
  !borderRight      = imgN - krnN2 - 1
  !getOutPx         = getOutFunc out
  getDims Crop _ _ _ _ = (imgM - krnM, imgN - krnN)
  getDims _    _ _ _ _ = (imgM, imgN)
  {-# INLINE getDims #-}
  getOutFunc Extend    = outExtend
  getOutFunc Wrap      = outWrap
  getOutFunc (Fill px) = outFill px
  getOutFunc Crop      = outCrop
  {-# INLINE getOutFunc #-}
  outExtend !getPx !i !j = getPx
                           (if i < 0 then 0 else if i >= imgM then imgM - 1 else i)
                           (if j < 0 then 0 else if j >= imgN then imgN - 1 else j)
  {-# INLINE outExtend #-}
  outWrap !getPx !i !j   = getPx (i `mod` imgM) (j `mod` imgN)
  {-# INLINE outWrap #-}
  outFill !px _ _ _      = px
  {-# INLINE outFill #-}
  outCrop !getPx !i !j   = getPx (i + krnM2) (j + krnN2)
  {-# INLINE outCrop #-}
  stencil !getKrnPx !getImgPx !i !j = integrate 0 0 0 where
    getImgPx' !i' !j' | j' < borderLeft  ||
                        j' > borderRight ||
                        i' < borderUp    ||  
                        i' > borderDown     = getOutPx getImgPx i' j'
                      | otherwise           = getImgPx i' j'
    {-# INLINE getImgPx' #-}
    !ikrnM = i - krnM2
    !jkrnN = j - krnN2
    integrate !ki !kj !acc
      | ki == krnM            = integrate 0 (kj+1) acc
      | ki == 0 && kj == krnN = acc
      | otherwise             = let !krnPx = getKrnPx ki kj
                                    !imgPx = getImgPx' (ki + ikrnM) (kj + jkrnN)
                                in integrate (ki+1) kj (acc + krnPx * imgPx)
    {-# INLINE integrate #-}
  {-# INLINE stencil #-}
    
{-# INLINE convolve #-}



convolveRows :: AImage img px => Outside px -> [px] -> img px -> img px
convolveRows !out !ls = convolve out . transpose . fromList $ [ls]
{-# INLINE convolveRows #-}


convolveCols :: AImage img px => Outside px -> [px] -> img px -> img px
convolveCols !out !ls = convolve out . fromList $ [ls]
{-# INLINE convolveCols #-}


