{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Graphics.Image.Processing.Filter
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Filter where

import Graphics.Image.Interface as I
import Graphics.Image.Processing.Convolution


gaussianVector :: (Array arr cs e, Floating e) =>
                  Int -- ^ Radius
               -> Image arr cs e
gaussianVector !r = gauss / scalar weight
  where
    !gauss = makeImage (1, n) getPx
    !weight = I.fold (+) 0 gauss
    !n = 2 * r + 1
    !sigma = fromIntegral r / 3
    !sigma2sq = 2 * sigma ** 2
    getPx (_, j) =
      promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
    {-# INLINE getPx #-}
{-# INLINE gaussianVector #-}


gaussian :: (Array arr cs t, Floating t, RealFrac t) =>
                 Int -> Image arr cs t -> Image arr cs t
gaussian !r = convolve Edge (transpose gV) . convolve Edge gV where
  !gV = compute $ gaussianVector r
