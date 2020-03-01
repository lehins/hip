{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Adaptive histogram equalization is used to improve contrast in images.
-- It adjusts image intensity in small regions (neighborhood) in the image.
--
-- /__Warning__/ - This module is experimental and likely doesn't work as expected
module Graphics.Image.Processing.Ahe where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.STRef

import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP

-- | Supplementary function for applying border resolution and a general mask.
simpleFilter :: (Array arr cs e, Array arr X e) => Direction -> Border (Pixel cs e) -> Filter arr cs e
simpleFilter dir !border =
  Filter (correlate border kernel)
  where
    !kernel =
      case dir of
        Vertical   -> fromLists [ [ 0, -1, 0 ], [ -1, 4, -1 ], [ 0, -1, 0 ] ]
        Horizontal -> fromLists [ [ 0, -1, 0 ], [ -1, 4, -1 ], [ 0, -1, 0 ] ]

-- | 'ahe' operates on small 'contextual' regions of the image. It enhances the contrast of each
-- region and this technique works well when the distribution of pixel values is similar throughout
-- the image.
--
-- The idea is to perform contrast enhancement in 'neighborhood region' of each pixel and the size
-- of the region is a parameter of the method. It constitutes a characteristic length scale: contrast
-- at smaller scales is enhanced, while contrast at larger scales is reduced (For general purposes, a size
-- factor of 5 tends to give pretty good results).
--
-- <<images/yield.jpg>>   <<images/yield_ahe.png>>
--
-- Usage :
--
-- >>> img <- readImageY VU "images/yield.jpg"
-- >>> input1 <- getLine
-- >>> input2 <- getLine
-- >>> let thetaSz = (P.read input1 :: Int)
-- >>> let distSz = (P.read input2 :: Int)
-- >>> let neighborhoodFactor = (P.read input2 :: Int)
-- >>> let aheImage = ahe img thetaSz distSz neighborhoodFactor :: Image VU RGB Double
-- >>> writeImage "images/yield_ahe.png" (toImageRGB aheImage)
--
ahe ::
     forall arr.
     ( MArray arr Y Double
     , IP.Array arr Y Double
     , IP.Array arr Y Word16
     , MArray arr Y Word16
     , Array arr X Double
     )
  => Image arr Y Double
  -> Int -- ^ width of output image
  -> Int -- ^ height of output image
  -> Int -- ^ neighborhood size factor
  -> Image arr Y Word16
ahe image thetaSz distSz neighborhoodFactor = I.map (fmap toWord16) accBin
 where
   ip = applyFilter (simpleFilter Horizontal Edge) image  -- Pre-processing (Border resolution)
   _widthMax, var1, _heightMax, var2 :: Int
   var1 = ((rows ip) - 1)
   _widthMax = ((rows ip) - 1)
   var2 = ((cols ip) - 1)
   _heightMax = ((cols ip) - 1)

   accBin :: Image arr Y Word16
   accBin = runST $                -- Core part of the Algo begins here.
     do arr <- I.new (thetaSz, distSz)   -- Create a mutable image with the given dimensions.
        forM_ [0 .. var1] $ \x -> do
          forM_ [0 .. var2] $ \y -> do
            rankRef <- newSTRef (0 :: Int)
            let neighborhood a maxValue = filter (\a -> a >= 0 && a < maxValue) [a-5 .. a+5]
            forM_ (neighborhood x var1) $ \i -> do
              forM_ (neighborhood y var2) $ \j -> do
                 when (I.index ip (x, y) > I.index ip (i, j)) $ modifySTRef' rankRef (+1)
            rank <- readSTRef rankRef
            let px = ((rank * 255))
            I.write arr (x, y) (PixelY (fromIntegral px))
        freeze arr
