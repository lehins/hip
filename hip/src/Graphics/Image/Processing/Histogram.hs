{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Image.Processing.Histogram
-- Copyright   : (c) Alexey Kuleshevich 2017-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Historgram (histogram, cdf, equalize) where

import Data.Coerce
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A
import Graphics.Image.Internal as I
import Graphics.Image.IO
import Graphics.Pixel as C


withNumBuckets ::
     forall e a. Elevator e
  => Maybe Word16
  -> Image Y e
  -> (Word16 -> A.Matrix A.D Int -> a)
  -> a
withNumBuckets mBuckets img f =
  case mBuckets of
    Nothing -> f (fromIntegral (maxBound :: Word8)) (A.map (fromIntegral . toWord8) darr)
    Just buckets ->
      let scaleBy = ((maxBound - 1) `quot` buckets) + 1
              -- account for integer division being a floor
       in f buckets (A.map (fromIntegral . (`quot` scaleBy) . toWord16) darr)
  where
    darr :: A.Matrix A.D e
    darr = A.map (coerce . pixelColor) $ delayPull img


histogram ::
     Elevator e
  => Maybe Word16
     -- ^ Number of buckets, can't be 0. Default is 255
  -> Image Y e
  -> A.Vector A.P Int
histogram mBuckets img = withNumBuckets mBuckets img histogramUnsafe


histogramUnsafe :: Source r ix Int => Word16 -> Array r ix Int -> A.Vector A.P Int
histogramUnsafe numBuckets m =
  A.createArrayST_ (fromIntegral numBuckets) $ \mvec ->
    A.forM_ m $ A.unsafeLinearModify mvec (pure . (+ 1))

-- | Compute [cumulative distribution function
-- (CDF)](https://en.wikipedia.org/wiki/Cumulative_distribution_function) of a the image
-- histogram
cdf :: (Elevator e, A.Prim a, Fractional a) => Maybe Word16 -> Image Y e -> A.Vector A.P a
cdf mBuckets img = withNumBuckets mBuckets img cdfUnsafe


cdfUnsafe ::
     (A.Prim e, Fractional e, Source r ix Int) => Word16 -> Array r ix Int -> A.Vector A.P e
cdfUnsafe numBuckets m = A.compute $ A.iunfoldrS_ (A.size h) collect 0
  where
    h = histogramUnsafe numBuckets m
    p = 1 / fromIntegral (A.elemsCount m)
    collect acc i =
      let !acc' = acc + fromIntegral (h `A.unsafeIndex` i)
       in (acc' * p, acc')

-- | Apply [histogram equalization](https://en.wikipedia.org/wiki/Histogram_equalization) to an image
equalize :: Elevator e => Maybe Word16 -> Image Y e -> Image Y Double
equalize mBuckets img =
  withNumBuckets mBuckets img $ \ numBuckets arr ->
    let arr' = A.computeAs A.S arr
        cdf' = cdfUnsafe numBuckets arr'
    in computeI $ A.map (\i -> PixelY (cdf' A.! i)) arr'
