{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

module Graphics.Image.Processing.Otsu where

import Control.Monad as CM (forM_, when)
import Control.Monad.ST
import Data.STRef 

import Prelude as P hiding (foldl', foldr)
import Graphics.Image.Processing.Filter
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP
import Graphics.Image.ColorSpace (X)
import Data.Vector.Unboxed as V
import Data.List as L

-- the tail is safe because scanl alawys returns a nonempty list
-- there's no way to get rid of it because what if dropWhile returns an empty list	
suppl :: V.Vector Int -> Int
suppl hist = thresh
  where
    (_, _, _, _, thresh) =
      L.head $
      L.dropWhile (\(_, wF, _, _, _) -> wF /= 0) $
      L.tail $ L.scanl' iteration (0, 0, 0, 0 :: Double, 0) [0 .. V.length hist - 1]
    total = V.sum hist
    sumHist = V.sum $ V.imap (\i v -> i * fromIntegral v) hist
    iteration (!wB', _wF, !sumB', !varMax', !threshold') t =
      let h = hist ! t
          wB = wB' + h
          wF = total - wB
          sumB = sumB' + (t * h)
          mB = fromIntegral sumB / fromIntegral wB
          mF = fromIntegral (sumHist - sumB) / fromIntegral wF
          varBetween = fromIntegral (wB * wF) * (mB - mF) ^ (2 :: Int)
          (varMax, threshold) =
            if varBetween > varMax'
              then (varBetween, t)
              else (varMax', threshold')
       in if wB == 0 || wF == 0
            then (wB, wF, sumB', varMax', threshold')
            else (wB, wF, sumB, varMax, threshold)


getThresh
  :: forall arr e cs . (RealFrac (Pixel X Double), RealFrac (Pixel Y Int), MArray arr X Double, IP.Array arr RGB Double, IP.MArray arr RGB Double, MArray arr X Bit)
  => Image arr X Double
  -> Int
  -> Image arr X Bit
getThresh image thresholdValue = intense
 where
   widthMax, heightMax :: Int
   widthMax = ((rows image) - 1)
   heightMax = ((cols image) - 1)
   intense :: Image arr X Bit
   intense = runST $
     do arr <- I.new (widthMax + 1, heightMax + 1)                
        CM.forM_ [0 .. widthMax] $ \x -> do
          CM.forM_ [0 .. heightMax] $ \y -> do
            let m = (I.index image (x, y)) 
            if (m <= (PixelX (fromIntegral thresholdValue))) 
               then do let px = 0
                       I.write arr (x, y) (PixelX (fromIntegral px)) 
               else do let px = 1
                       I.write arr (x, y) (PixelX (fromIntegral px)) 
        I.freeze arr

otsu
  :: forall arr e cs . (RealFrac (Pixel X Double), RealFrac (Pixel Y Int), MArray arr X Double, IP.Array arr RGB Double, IP.Array arr RGB Double)
  => Image arr X Double
  -> Image arr X Bit
otsu image = accBin
 where
   hist  = hBins (getHistogram image)
   thresh = suppl (hist)     
   accBin :: Image arr X Bit 
   accBin = getThresh (image, thresh) 

     


