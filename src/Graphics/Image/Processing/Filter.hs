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

import           Data.Maybe                            (fromMaybe)
import           Graphics.Image.Interface              as I
import           Graphics.Image.Processing.Convolution


data Filter arr cs e = Filter
  { applyFilter :: Image arr cs e -> Image arr cs e
  }


-- | Create a Gaussian Blur filter
gaussianBlur :: (Array arr cs e, Floating e, RealFrac e) =>
                Int -- ^ Radius
             -> Maybe e -- ^ Sigma
             -> Filter arr cs e
gaussianBlur !r !mSigma = Filter (correlate Edge (transpose gV) . correlate Edge gV)
  where
    !gV = compute $ (gauss / scalar weight)
    !gauss = makeImage (1, n) getPx
    !weight = I.fold (+) 0 gauss
    !n = 2 * r + 1
    !sigma = fromMaybe (fromIntegral r / 3) mSigma
    !sigma2sq = 2 * sigma ** 2
    getPx (_, j) =
      promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
    {-# INLINE getPx #-}
{-# INLINE gaussianBlur #-}


-- -- | Create a Gaussian Blur filter
-- gaussianBandpass :: (Array arr cs e, Floating e, RealFrac e) =>
--                 Int -- ^ Radius
--              -> Maybe e -- ^ Sigma
--              -> Filter arr cs e
-- gaussianBandpass !r !mSigma = Filter (correlate Edge (transpose gV) . correlate Edge gV)
--   where
--     !gV = compute $ (gauss / scalar weight)
--     !gauss = makeImage (1, n) getPx
--     !weight = I.fold (+) 0 gauss
--     !n = 2 * r + 1
--     !sigma = fromMaybe (fromIntegral r / 3) mSigma
--     !sigma2sq = 2 * sigma ** 2
--     getPx (_, j) =
--       promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
--     {-# INLINE getPx #-}
-- {-# INLINE gaussianBandpass #-}
