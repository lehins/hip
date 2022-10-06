{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.Processing.Histogram
-- Copyright   : (c) Alexey Kuleshevich 2017-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Histogram
  ( Histogram(..)
  , histogram
  , Histograms(..)
  , histograms
  , cdf
  , equalize
  , equalizeGrayscale
  ) where

import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A
import Graphics.Image.Internal as I
import Graphics.Pixel as CM
import Data.List.NonEmpty as NE
import Data.Proxy

data Histogram = Histogram
  { histogramBins :: A.Vector S Int
    -- ^ Vector containing counts of pixels with the same value. Index of a vector serves
    -- as the original pixel value. Length of the vector is the number of bins that was
    -- used to compute the histogram
  , histogramName :: String
    -- ^ Name of the channel
  , histogramColor :: Color (Alpha RGB) Word8
    -- ^ Color of a plotted line. sRGB color space with Alpha channel.
  }

newtype Histograms = Histograms (NonEmpty Histogram)

withNumBuckets ::
     forall r e a. (Source r e, Elevator e)
  => Maybe Word16
  -> A.Matrix r e
  -> (Word16 -> A.Matrix A.D Int -> a)
  -> a
withNumBuckets mBuckets arr f =
  case mBuckets of
    Nothing -> f (fromIntegral (maxBound :: Word8)) (A.map (fromIntegral . toWord8) arr)
    Just buckets
      | buckets == 0 -> error "Number of buckets can't be zero"
      | otherwise ->
        let scaleBy = ((maxBound - 1) `quot` buckets) + 1
                    -- account for integer division being a floor
         in f buckets (A.map (fromIntegral . (`quot` scaleBy) . toWord16) arr)


histogram ::
     forall cs e. (ColorModel cs e, ChannelCount cs ~ 1)
  => Maybe Word16
     -- ^ Number of buckets. Default is 255 and can't be 0
  -> Image cs e
  -> Histogram
histogram mBuckets img =
  Histogram
    { histogramBins = withNumBuckets mBuckets (grayImageToArray img) histogramUnsafe
    , histogramName = channelName
    , histogramColor = Alpha channelColor 255
    }
  where
    proxy = Proxy :: Proxy (Color cs e)
    channelName NE.:| _ = channelNames proxy
    channelColor NE.:| _ = channelRgbColors proxy

histograms ::
     forall cs e. ColorModel cs e
  => Maybe Word16
     -- ^ Number of buckets, can't be 0. Default is 255
  -> Image cs e
  -> Histograms
histograms mBuckets img = Histograms $ NE.zipWith mkHistogram chIxs legend
  where
    arr = imageToArrayOfChannels img
    proxy = Proxy :: Proxy (Color cs e)
    legend = NE.zip (channelNames proxy) (channelRgbColors proxy)
    chIxs = 0 NE.:| [1 .. fromIntegral (channelCount proxy)]
    mkHistogram channelIx (channelName, channelColor) =
      Histogram
        { histogramBins = withNumBuckets mBuckets (arr A.<! channelIx) histogramUnsafe
        , histogramName = channelName
        , histogramColor = Alpha channelColor 255
        }


histogramUnsafe :: (Index ix, Source r Int) => Word16 -> Array r ix Int -> A.Vector S Int
histogramUnsafe numBuckets m =
  A.createArrayST_ (fromIntegral numBuckets) $ \mvec ->
    A.forM_ m $ A.unsafeLinearModify mvec (pure . (+ 1))

-- | Compute [cumulative distribution function
-- (CDF)](https://en.wikipedia.org/wiki/Cumulative_distribution_function) of an
-- image histogram
cdf :: (Elevator e, Elevator a, Fractional a) => Maybe Word16 -> Image X e -> A.Vector S a
cdf mBuckets img = withNumBuckets mBuckets (grayImageToArray img) cdfUnsafe


cdfUnsafe ::
     (Elevator e, Fractional e, Index ix, Source r Int) => Word16 -> Array r ix Int -> A.Vector S e
cdfUnsafe numBuckets m = A.compute $ A.iunfoldrS_ (A.size h) collect 0
  where
    h = histogramUnsafe numBuckets m
    p = fromIntegral (A.elemsCount m)
    collect acc i =
      let !acc' = acc + fromIntegral (h `A.unsafeIndex` i)
       in (acc' / p, acc')

-- | Apply [histogram equalization](https://en.wikipedia.org/wiki/Histogram_equalization) to an image
equalizeGrayscale :: Elevator e => Maybe Word16 -> Image X e -> Image X e
equalizeGrayscale mBuckets img =
  withNumBuckets mBuckets (grayImageToArray img) $ \ numBuckets arr ->
    let arr' = A.computeAs A.S arr
        cdf' = cdfUnsafe numBuckets arr'
    in computeI $ A.map (\i -> CM.PixelX (fromDouble (cdf' `A.unsafeIndex` i))) arr'


equalize :: ColorSpace cs i e => Maybe Word16 -> Image cs e -> Image cs e
equalize mBuckets img = applyImageGrayscale img (equalizeGrayscale mBuckets)
