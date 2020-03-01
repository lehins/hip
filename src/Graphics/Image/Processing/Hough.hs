{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hough Transform is used as a part of feature extraction in images.
-- It is a tool that makes it far easier to identify straight lines in
-- the source image, whatever their orientation.
--
-- /__Warning__/ - This module is experimental and likely doesn't work as expected
module Graphics.Image.Processing.Hough where

import Control.Monad (forM_, when)
import qualified Data.Foldable as F (maximum)
import Data.Array
import Data.Array.ST (newArray, writeArray, readArray, runSTArray)

import Prelude as P hiding (subtract)
import Graphics.Image
import Graphics.Image.Interface as I
import Graphics.Image.Types as IP

-- | Some helper functions :
-- | Trivial function for subtracting co-ordinate pairs
sub :: Num x => (x, x) -> (x, x) -> (x, x)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Compute the sum of squares or dot product of a given pair of co-ordinates
dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

-- | Conversion of pair fromIntegral
fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- | Compute magnitude
mag :: Floating x => (x, x) -> x
mag x = sqrt (dotProduct x x)

-- | 'hough' computes the Linear Hough Transform and maps each point in the target image, (ρ, θ)
-- to the average color of the pixels on  the corresponding line of the source image (x,y) - space,
-- where the line corresponds to points of the form (xcosθ + ysinθ = ρ(rho)).
--
-- The idea is that where there is a straight line in the original image, it corresponds to a
-- bright (or dark, depending on the color of the background field) spot; by applying a suitable
-- filter to the results of the transform, it is possible to extract the locations of the lines in the original image.
--
-- <<images/yield.jpg>>   <<images/yield_hough.png>>
--
-- Usage :
--
-- >>> yield <- readImageRGB VU "yield.jpg"
-- >>> input1 <- getLine
-- >>> input2 <- getLine
-- >>> let thetaSz = (P.read input1 :: Int)
-- >>> let distSz = (P.read input2 :: Int)
-- >>> let houghImage :: Image VU RGB Double
-- >>>     houghImage = hough yield thetaSz distSz
-- >>> writeImage "test.png" houghImage
--
hough
  :: forall arr . ( MArray arr Y Double, IP.Array arr Y Double, IP.Array arr Y Word8)
  => Image arr Y Double
  -> Int
  -> Int
  -> Image arr Y Word8
hough image thetaSz distSz = I.map (fmap toWord8) hImage
 where
   widthMax, xCtr, heightMax, yCtr :: Int
   widthMax = ((rows image) - 1)
   xCtr = (widthMax `div` 2)
   heightMax = ((cols image) - 1)
   yCtr = (heightMax `div` 2)

   slope :: Int -> Int -> (Double, Double)
   slope x y =
     let PixelY orig = I.index image (x, y)
         PixelY x' = I.index image (min (x+1) widthMax, y)
         PixelY y' = I.index image (x, min (y+1) heightMax)
     in (orig - x', orig - y')

   slopeMap :: [ ((Int, Int), (Double, Double)) ]
   slopeMap = [ ((x, y), slope x y) | x <- [0 .. widthMax], y <- [0 .. heightMax] ]

   distMax :: Double -- Compute Maximum distance
   distMax = (sqrt . fromIntegral $ (heightMax + 1) ^ (2 :: Int) + (widthMax + 1) ^ (2 :: Int)) / 2

   accBin = runSTArray $   -- Core part of Algo begins here. Working in a safe way with a mutable array.
     do arr <- newArray ((0, 0), (thetaSz, distSz)) (0 :: Double) -- Build a new array, with every element initialised to the supplied value.
        forM_ slopeMap $ \((x, y), gradient) -> do
            let (x', y') = fromIntegralP $ (xCtr, yCtr) `sub` (x, y)
            when (mag gradient > 0) $
              forM_ [0 .. thetaSz] $ \theta -> do
                let theta_ =
                      fromIntegral theta * 360 / fromIntegral thetaSz / 180 *
                      pi :: Double
                    distance = cos theta_ * x' + sin theta_ * y'    -- (ρ(rho) = xcosθ + ysinθ)
                    distance_ = truncate $ distance * fromIntegral distSz / distMax -- returns the nearest integer
                    idx = (theta, distance_)
                when (distance_ >= 0 && distance_ < distSz) $
                  do old <- readArray arr idx      -- read an element at 'idx' from mutable array 'arr'
                     writeArray arr idx (old + 1)
        return arr

   maxAcc = F.maximum accBin
   hTransform (x, y) =
        let l = 255 - truncate ((accBin ! (x, y)) / maxAcc * 255) -- pixel generating function
        in PixelY l

   hImage :: Image arr Y Word8
   hImage = makeImage (thetaSz, distSz) hTransform
