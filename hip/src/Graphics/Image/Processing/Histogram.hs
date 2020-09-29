{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
module Graphics.Image.Processing.Histogram
  ( Histogram(..)
  , Histograms(..)
  , histogram
  , cdf
  , equalize
  ) where

import Data.Coerce
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A
import Graphics.Image.Internal as I
import Graphics.Image.IO
import Graphics.Pixel as CM
import Graphics.Color.Space (Linearity(..))
import Graphics.Color.Model as CM
import Data.List.NonEmpty as NE

data Histogram = Histogram
  { histogramBins :: A.Vector S Int
    -- ^ Vector containing counts of pixels with the same value. Index of a vector serves
    -- as the original pixel value. Length of the vector is the number of bins that was
    -- used to compute the histogram
  , histogramName :: String
    -- ^ Name of the channel
  , histogramColor :: Color (Alpha CM.RGB) Double
    -- ^ Color of a plotted line. Assumed to be in sRGB color space with Alpha channel.
  }

newtype Histograms = Histograms (NonEmpty Histogram)

withNumBuckets ::
     forall e a. Elevator e
  => Maybe Word16
  -> Image CM.Y e
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
  -> Image CM.Y e
  -> Histogram
histogram mBuckets img =
  Histogram
    { histogramBins = withNumBuckets mBuckets img histogramUnsafe
    , histogramName = "Luma" -- TODO get it from the actual color space
    , histogramColor = Alpha 0.5 1
    }



histogramUnsafe :: Source r ix Int => Word16 -> Array r ix Int -> A.Vector S Int
histogramUnsafe numBuckets m =
  A.createArrayST_ (fromIntegral numBuckets) $ \mvec ->
    A.forM_ m $ A.unsafeLinearModify mvec (pure . (+ 1))

-- | Compute [cumulative distribution function
-- (CDF)](https://en.wikipedia.org/wiki/Cumulative_distribution_function) of a the image
-- histogram
cdf :: (Elevator e, Elevator a, Fractional a) => Maybe Word16 -> Image CM.Y e -> A.Vector S a
cdf mBuckets img = withNumBuckets mBuckets img cdfUnsafe


cdfUnsafe ::
     (Elevator e, Fractional e, Source r ix Int) => Word16 -> Array r ix Int -> A.Vector S e
cdfUnsafe numBuckets m = A.compute $ A.iunfoldrS_ (A.size h) collect 0
  where
    h = histogramUnsafe numBuckets m
    p = 1 / fromIntegral (A.elemsCount m)
    collect acc i =
      let !acc' = acc + fromIntegral (h `A.unsafeIndex` i)
       in (acc' * p, acc')

-- | Apply [histogram equalization](https://en.wikipedia.org/wiki/Histogram_equalization) to an image
equalize :: (Elevator a, Elevator e, Fractional e) => Maybe Word16 -> Image CM.Y a -> Image CM.Y e
equalize mBuckets img =
  withNumBuckets mBuckets img $ \ numBuckets arr ->
    let arr' = A.computeAs A.S arr
        cdf' = cdfUnsafe numBuckets arr'
    in computeI $ A.map (\i -> CM.PixelY (cdf' `A.unsafeIndex` i)) arr'
