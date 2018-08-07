{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

module Graphics.Image.Processing.Noise where

import Control.Monad (forM_)
import Control.Monad.ST
import System.Random

import Prelude as P hiding (subtract)
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP
import Graphics.Image.ColorSpace (X)

-- | Helper function for generating a list of random co-ordinates.
randomCoords :: StdGen -> Int -> Int -> [(Int,Int)]
randomCoords a width height = (rnx1, rny1) : randomCoords g2 width height
  where
    (rnx1, g1) = randomR (1, width) a
    (rny1, g2) = randomR (1, height) g1

-- | Salt and pepper noise or impulse noise is a form of noise seen on images.
-- It is mainly caused by sharp and sudden disturbances in the image signal. 
-- 
-- 'snp' generates this particular type of noise by introducing a sparse 
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
-- >>> input2 <- getLine
-- >>> input3 <- getLine
-- >>> g <- newStdGen
-- >>> let thetaSz = (P.read input1 :: Int)
-- >>> let distSz = (P.read input2 :: Int) 
-- >>> let noiseLevel = (P.read input3 :: Float)
-- >>> let snpImage :: Image VU Y Word16
-- >>>     snpImage = snp frog thetaSz distSz noiseLevel g
-- >>> writeImage "images/yield_snp.png" (toImageRGB snpImage)
--
snp
  :: forall arr e cs . (MArray arr Y Double, IP.Array arr Y Double, IP.Array arr Y Word16, MArray arr Y Word16)
  => Image arr Y Double
  -> Int    -- ^ width of output image
  -> Int    -- ^ height of output image
  -> Float  -- ^ Noise Intensity -> Domain : (0, 1)
  -> StdGen -- ^ Instance of RandomGen
  -> Image arr Y Word16
snp image thetaSz distSz noiseLevel gen  = I.map (fmap toWord16) (accBin gen)
 where
   widthMax, heightMax, noiseIntensity :: Int
   widthMax = ((rows image) - 1)
   heightMax = ((cols image) - 1)
   noiseIntensity = round (noiseLevel * (fromIntegral widthMax) * (fromIntegral heightMax))   

   accBin :: StdGen -> Image arr Y Double
   accBin g = runST $ 
      do arr <- I.new (thetaSz, distSz)   
         let coords = take (noiseIntensity + 1) (randomCoords g widthMax heightMax)
         forM_ [0 .. widthMax] $ \x -> do
           forM_ [0 .. heightMax] $ \y -> do
             let px = I.index image (x, y)   
             I.write arr (x, y) px
         forM_ coords $ \i -> do
           let a :: Int
               a = uncurry (+) i        
           if (a `mod` 2 == 0)
              then do let px =  0
                      I.write arr i (PixelY (fromIntegral px))
              else do let px = 255
                      I.write arr i (PixelY (fromIntegral px))
         freeze arr

