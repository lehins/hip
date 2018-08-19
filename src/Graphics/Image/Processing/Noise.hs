{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

module Graphics.Image.Processing.Noise where

import Control.Monad (forM_)
import Control.Monad.ST
import System.Random
import Data.Random.Normal
import Data.Array.MArray

import Prelude as P hiding (subtract)
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP

-- | Helper function for generating a list of random co-ordinates.
randomCoords :: StdGen -> Int -> Int -> [(Int,Int)]
randomCoords a width height = (rnx1, rny1) : randomCoords g2 width height
  where
    (rnx1, g1) = randomR (0, width) a
    (rny1, g2) = randomR (0, height) g1

-- | Salt and pepper noise or impulse noise is a form of noise seen on images.
-- It is mainly caused by sharp and sudden disturbances in the image signal. 
-- 
-- 'saltAndPepper' generates this particular type of noise by introducing a sparse 
-- distribution of white and black pixels in the input image. The level or intensity
-- of the noise to be introduced is a parameter of this method and is scaled 
-- between 0 and 1, that is the input Noise Intensity has a domain : (0, 1).
-- 
-- <<images/yield.jpg>>   <<images/yield_snp.png>>
--
-- Usage :
-- 
-- >>> img <- readImageY VU "images/yield.jpg"
-- >>> input1 <- getLine
-- >>> g <- newStdGen
-- >>> let noiseLevel = (P.read input1 :: Float)
-- >>> let snpImage :: Image VU Y Double
-- >>>     snpImage = saltAndPepper img noiseLevel g
-- >>> writeImage "images/yield_snp.png" snpImage
--
saltAndPepper
  :: forall arr e cs . (IP.MArray arr Y Double, IP.Array arr Y Double)
  => Image arr Y Double
  -> Float  -- ^ Noise Intensity -> Domain : (0, 1)
  -> StdGen -- ^ Instance of RandomGen
  -> Image arr Y Double
saltAndPepper image noiseLevel = accBin
 where
   widthMax, heightMax, noiseIntensity :: Int
   widthMax = ((rows image) - 1)
   heightMax = ((cols image) - 1)
   noiseIntensity = round (noiseLevel * (fromIntegral widthMax) * (fromIntegral heightMax))   

   accBin :: StdGen -> Image arr Y Double
   accBin g = runST $ 
      do arr <- I.thaw image  
         let coords = take (noiseIntensity + 1) (randomCoords g widthMax heightMax)
         forM_ coords $ \i -> do
           let a :: Int
               a = uncurry (+) i        
           if (a `mod` 2 == 0)
              then do let px =  0
                      I.write arr i px
              else do let px = 1.0
                      I.write arr i px
         I.freeze arr

gaussianNoise
  :: forall arr e cs . (IP.MArray arr Y Double, IP.Array arr Y Double)
  => Image arr Y Double
  -> Float  -- ^ Mean
  -> Float  -- ^ Sigma
  -> StdGen -- ^ Instance of RandomGen
  -> Image arr Y Double
gaussianNoise image mean sigma g = accBin 
 where
   widthMax, heightMax :: Int
   widthMax = ((rows image) - 1)
   heightMax = ((cols image) - 1)
   samples = normals' (mean, sigma) g
   new = newListArray (widthMax, heightMax) samples

   accBin :: [(Int,Int)] -> Image arr Y Double
   accBin new = runST $ 
      do arr <- I.thaw image  
         forM_ [0 .. widthMax] $ \x -> do
          forM_ [0 .. heightMax] $ \y -> do  
              let value = (fromIntegral (readArray new (x, y) ))
              let px = ( (I.index image (x, y)) + (PixelY value) )
              I.write arr (x, y) px
         I.freeze arr


