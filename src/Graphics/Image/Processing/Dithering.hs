{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

-- | Adaptive histogram equalization is used to improve contrast in images.
-- It adjusts image intensity in small regions (neighborhood) in the image.
module Graphics.Image.Processing.Dithering where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.STRef 


import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP
import Graphics.Image.ColorSpace (X)


applyThreshold :: RealFrac x => x -> x
applyThreshold x = 255 * fromIntegral (floor(x/128))

dithering
  :: forall arr e cs . ( MArray arr RGB Double, IP.Array arr RGB Double, IP.Array arr RGB Double)
  => Image arr RGB Double
  -> Int  -- ^ width of output image
  -> Int  -- ^ height of output image 
  -> Image arr RGB Double
dithering image thetaSz distSz = accBin
 where
   widthMax, heightMax :: Int
   widthMax = ((rows image) - 1)
   heightMax = ((cols image) - 1)
    
   accBin :: Image arr RGB Double
   accBin = runST $                
     do arr <- I.new (thetaSz, distSz) 
        forM_ [0 .. widthMax] $ \x -> do
          forM_ [0 .. heightMax] $ \y -> do
            let m = (I.index image (x, y))
            let re = m - 0.1
            let be = m - 0.5
            let ge = m - 0.85
            let err1 = m - re 
            let err2 = m - be
            let err3 = m - ge
            if (x < widthMax) 
               then do let px = (re * 7/16)
                       I.write arr (x+1, y) px 
               else if (x > 1 && y < heightMax)
                       then do let px = (ge * 3/16)
                               I.write arr (x-1, y+1) px
               else if (x > 1 && y < heightMax)
                       then do let px = (be * 5/16)
                               I.write arr (x, y+1) px
               else if (x > 1 && y < heightMax)
                       then do let px = (ge * 1/16)
                               I.write arr (x-1, y+1) px
               else do let px = I.index image (x, y)
                       I.write arr (x, y) px 
        freeze arr 

test :: IO ()
test = do
      frog <- readImageRGB VU "yield.jpg"
      input1 <- getLine
      input2 <- getLine
      let thetaSz = (P.read input1 :: Int)
      let distSz = (P.read input2 :: Int)
      writeImage "input.png" frog
      let ditherImage :: Image VU RGB Double
          ditherImage = dithering frog thetaSz distSz
      writeImage "dither.png" ditherImage  

