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
  :: forall arr e cs . ( MArray arr Y Double, IP.Array arr Y Double, IP.Array arr Y Double, IP.Array arr Y Word8)
  => Image arr Y Double
  -> Image arr Y Word8
dithering image = I.map (fmap toWord8) accBin
 where
   widthMax, heightMax :: Int
   widthMax = ((rows image) - 1)
   heightMax = ((cols image) - 1)
    
   accBin :: Image arr Y Double
   accBin = runST $                
     do arr <- I.new (widthMax + 1, heightMax + 1) 
        forM_ [0 .. widthMax] $ \x -> do
          forM_ [0 .. heightMax] $ \y -> do
            let m = (I.index image (x, y))
{-            if (m > 0.5)
               then do let we = 1 - m
                       I.write arr (x, y) m
               else if (m <= 0.5)
                       then do let be = m - 0
                               I.write arr (x, y) m
               else do let we = 1 - m
                       I.write arr (x, y) m -}
            if (x < widthMax) 
               then do let px = ( (1-m) * 7/16)
                       I.write arr (x+1, y) px 
               else if (x > 1 && y < heightMax)
                       then do let px = ((m-0) * 3/16)
                               I.write arr (x-1, y+1) px
               else if (x > 1 && y < heightMax)
                       then do let px = ((1-m) * 5/16)
                               I.write arr (x, y+1) px
               else if (x > 1 && y < heightMax)
                       then do let px = ((m-0) * 1/16)
                               I.write arr (x-1, y+1) px
               else do let px = I.index image (x, y)
                       I.write arr (x, y) px 
        freeze arr 

test :: IO ()
test = do
      frog <- readImageY VU "yield.jpg"
      writeImage "input.png" frog
      let ditherImage :: Image VU Y Word8
          ditherImage = dithering frog
      writeImage "dither2.png" (toImageRGB ditherImage)  

