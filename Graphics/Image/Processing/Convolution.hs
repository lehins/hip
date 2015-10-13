{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Processing.Convolution (
  Outside(..), convolve
  ) where

import Graphics.Image.Interface

{- | Approach to be used near the border during convolution. -}
data Outside px =
  Extend      -- ^ Extend the input image by repeating the border rows and
              -- columns.
  | Wrap      -- ^ Wrap the input image (thereby making it periodic) by assuming
              -- the first column comes immediately after the last, etc.
  | Fill !px  -- ^ Fill in a constant pixel for output pixels too near the
              -- border
  | Crop      -- ^ Compute an image of redused size, by eliminating output rows
              -- and columns that cannot be computed by convolution.
  deriving Eq



convolve  :: (Image img px, Pixel px) =>
              Outside px   -- ^ Approach to be used near the borders.
           -> img px       -- ^ Convolution kernel image.
           -> img px       -- ^ Image that will be used to convolve the kernel.
           -> img px
convolve !out !kernel !img = traverse2 kernel img getDims stencil where
  !(krnM, krnN)     = dims kernel        
  !(imgM, imgN)     = dims img
  !krnM2@borderUp   = krnM `div` 2
  !krnN2@borderLeft = krnN `div` 2
  !borderDown       = imgM - krnM2 - 1
  !borderRight      = imgN - krnN2  - 1
  !getOutPx         = getOutFunc out
  getDims _ _ _ _   = getDims' out
  getDims' Crop     = (imgM - krnM, imgN - krnN)
  getDims' _        = (imgM, imgN)
  getOutFunc Extend    = outExtend
  getOutFunc Wrap      = outWrap
  getOutFunc (Fill px) = outFill px
  getOutFunc Crop      = outCrop
  {-# INLINE getOutFunc #-}
  outExtend !getPx !i !j = getPx
                           (if i < 0 then 0 else if i >= imgM then (imgM-1) else i)
                           (if j < 0 then 0 else if j >= imgN then (imgN-1) else j)
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


{-
convolve :: (Strategy strat img px, Image img px, Pixel px, Num px) =>
            Image px
         -> Image px
         -> Image px
{-# INLINE convolve #-}
convolve strat = convolveOut strat Wrap

convolve' :: Pixel px => Image px -> Image px -> Image px
{-# INLINE convolve' #-}
convolve' = convolveOut Extend

convolveRows :: Pixel px => [px] -> Image px -> Image px
{-# INLINE convolveRows #-}
convolveRows row = convolve . transpose . fromLists $ [row]

convolveCols :: Pixel px => [px] -> Image px -> Image px
{-# INLINE convolveCols #-}
convolveCols row = convolve . fromLists $ [row]



convolveOut :: (Num px, Pixel px, Image img px) =>
               ((Int -> Int -> px)
                -> Int -> Int
                -> px)  -- ^ How to handle out-of-range elements.
            -> img px -- ^ Kernel to use in the convolution.
            -> img px -- ^ Input image.
            -> img px
convolveOut getOut kernel img = traverse img (,) stencil
 where  
        !(krnM, krnN)     = dims kernel        
        !(imgM, imgN)     = dims img
        !krnM2@borderUp   = krnM `div` 2
        !krnN2@borderLeft = krnN `div` 2
        !borderDown       = imgM - krnM2 - 1
        !borderRight      = imgN - krnN2  - 1
        stencil !getPx !i !j = integrate 0 0 0 where
          getPx' !i' !j' | j' < borderLeft  ||
                           j' > borderRight ||
                           i' < borderUp    ||  
                           i' > borderDown     = getOut getPx i' j'
                         | otherwise           = getPx i' j'
          {-# INLINE getPx' #-}
          !ikrnM = i - krnM2
          !jkrnN = j - krnN2

          integrate !ki !kj !acc
            | ki == krnM            = integrate 0 (kj+1) acc
            | ki == 0 && kj == krnN = acc
            | otherwise             = let !krnPx = ref kernel ki kj
                                          !imgPx = getPx' (ki + ikrnM) (kj + jkrnN)
                                      in integrate (ki+1) kj (acc + krnPx * imgPx)
          {-# INLINE integrate #-}
        {-# INLINE stencil #-}

{-# INLINE convolveOut #-}


convolve  :: (Strategy strat img px, Image img px, Pixel px, Num px) =>
             strat img px -- ^ Computation strategy to be applied.
          -> Outside px   -- ^ Approach to be used near the borders.
          -> img px       -- ^ Convolution kernel image.
          -> img px       -- ^ Image that will be used to convolve the kernel.
          -> img px
convolve strat out kernel@(dims -> (m', n')) img@(dims -> (m, n)) =
  compute strat $ convolveOut (getOutFunc out) (compute strat kernel) (getImg out)
  where
    getImg Crop = compute strat $ crop krnM2 krnN2 (m-m') (n-n') img
    getImg _    = computedImage
    !computedImage = compute strat img
    (krnM2, krnN2) = (m' `div` 2, n' `div` 2)
    outExtend !getPx !i !j = getPx
                             (if i < 0 then 0 else if i >= m then (m-1) else i)
                             (if j < 0 then 0 else if j >= n then (n-1) else j)
    {-# INLINE outExtend #-}
    outWrap !getPx !i !j   = getPx (i `mod` m) (j `mod` n)
    {-# INLINE outWrap #-}
    outFill !px _ _ _      = px
    {-# INLINE outFill #-}
    outCrop _ !i !j        = ref computedImage (i + krnM2) (j + krnN2)
    {-# INLINE outCrop #-}
    getOutFunc Extend    = outExtend
    getOutFunc Wrap      = outWrap
    getOutFunc (Fill px) = outFill px
    getOutFunc Crop      = outCrop
    {-# INLINE getOutFunc #-}
{-# INLINE convolve #-}

-}
