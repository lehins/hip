{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Processing.HoughTransform where

import Control.Applicative
import System.Environment (getArgs, getProgName)
import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Foldable as F (maximum)
-- import Data.Massiv.Array.IO
import Data.List
import Data.Array.MArray

import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter	
import Graphics.Image as GI
import Graphics.Image.ColorSpace
import Graphics.Image.IO
import Graphics.Image.Interface as I 
import Graphics.Image.Types as IP

-- ######### Read Image ##########
-- readImageRGB :: Array arr RGB Double => arr -> FilePath -> IO (Image arr RGB Double)
-- readImageRGB _ = readImage'

-- frog <- readImageRGB VU "images/frog.jpg"
-- writeImage "images/frog_eye_grid.png" $ pixelGrid 10 $ crop (51, 112) (20, 20) frog

-- makeImage :: Array arr cs e => (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e 

-- ######## Convert to Luma #########

toImageY :: (ToY cs e, Array arr cs e, Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = I.map toPixelY

-- ####### Some trivial functions ########
subtract :: Num x => (x, x) -> (x, x) -> (x, x)
subtract (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)
 
mag :: Floating x => (x, x) -> x
mag x = sqrt (dotProduct x x)

fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

{-
-- trying something new - Functor usage!
instance Functor P where
    fmap f (x :| y) = f x :| f y
 
fromIntegralP :: (Integral a, Num b) => P a -> P b
fromIntegralP = fmap fromIntegral
-}

-- ######## Hough-T function begins

hough :: Image arr RGBA a -> Int -> Int -> Image arr RGBA a
hough image thetaSize distSize = hImage
 where
  widthMax = (GI.rows image) - 1
  heightMax = (GI.cols image) - 1
  xCtr = widthMax / 2
  yCtr = heightMax / 2
  --luma = IP.toImageY image

  arr = makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200) 
  luma = fmap PixelY arr

  slope x y =
     let orig = I.index luma (xCtr, yCtr) 
         x_ = I.index luma (widthMax,y)		 
         y_ = I.index luma (x,heightMax)
     in fromIntegralP (orig - x_, orig - y_)
  -- List
  slopeMap = [ ((x, y), slope x y) | x <- [0 .. widthMax], y <- [0 .. heightMax] ]
  
  -- Type declaration
  distMax :: Double
  distMax = (sqrt . fromIntegral $ (heightMax + 1) ^ 2 + (widthMax + 1) ^ 2) / 2
  
  minLineLength :: Int
  minLineLength = 100
   
  maxLineGap :: Int
  maxLineGap = 10

   
  accBin = runST $
     do arr <- newArray ((0, 0), (thetaSize, distSize)) 0
        forM_ slopeMap $ \((x, y), gradient) -> do
            let (x_, y_) = fromIntegralP ((xCtr, yCtr) `subtract` (x, y))
            when ((mag gradient) > 127) $
              forM_ [0 .. thetaSize] $ \theta -> do
                let theta_ =
                      fromIntegral theta * 360 / fromIntegral thetaSize / 180 *
                      pi :: Double
                    distance = round (cos theta_ * x_ + sin theta_ * y_) * ( distSize / fromIntegral distMax)
                    --fromIntegral distance_ = round  (distance * fromIntegral distSize )/ distMax
                    idx = (theta, distance)
				-- optimization possible 
				-- minLineLength = 100 (pixels) and maxLineGap = 10 (pixels)
                when (distance>= 0 && distance < distSize) $
                  do old <- readArray arr idx
                     writeArray arr idx (old + 1)
        return arr

  maxAcc = F.maximum accBin 
  -- Generating function
  hTransform x y =
       let l = 255 - round ((I.index accBin (x, y)) /255 ) * maxAcc
       in PixelRGBA l l l l
  hImage = makeImage (thetaSize, distSize) hTransform

houghIO :: FilePath -> FilePath -> Int -> Int -> IO ()
houghIO path outpath thetaSize distSize = do
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right image_ -> doImage image_
    _ -> putStrLn "Unexpected Pixel Format"
  where
    doImage :: Image VS RGBA Double -> IO ()
    doImage image = do
      let houghImage = hough image thetaSize distSize
      writeImage outpath houghImage

{- ######### Helper functions #########
 transpose :: Array arr cs e => Image arr cs e -> Image arr cs e 
 index :: MArray arr cs e => Image arr cs e -> (Int, Int) -> Pixel cs e  -- Pixel at ith, jth
 dims :: BaseArray arr cs e => Image arr cs e -> (Int, Int)  -- get dimensions of image
 	>>> frog <- readImageRGB VU "images/frog.jpg"
	>>> frog
	<Image VectorUnboxed RGB (Double): 200x320>
	>>> dims frog
	(200,320)

 displayImage :: (Array VS cs e, Array arr cs e, Writable (Image VS cs e) TIF) => Image arr cs e -> IO ()
 writeImage :: (Array VS cs e, Array arr cs e, Writable (Image VS cs e) OutputFormat) => FilePath -> Image arr cs e	-> IO ()
 
 instance ToY RGB where
  toPixelY (PixelRGB r g b) = PixelY (0.299*r + 0.587*g + 0.114*b)
 
 toImageBinary :: (Array arr cs e, Array arr Binary Bit, Eq (Pixel cs e)) => Image arr cs e -> Image arr Binary Bit
  toImageBinary = I.map toPixelBinary

 makeImage :: Array arr cs e => (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e  -- Given generating function and dimensions
 pixelAt :: Image a -> Int -> Int -> a 

-}

{-
main :: IO ()
main = do 
 args <- getArgs
 prog <- getProgName
 case args of
   [path, outpath, thetaSize, distSize] ->
     houghIO path outpath (P.read thetaSize) (P.read distSize)
   _ ->
     putStrLn $
     "Usage: " ++ prog ++ " <image-file> <out-file.png> <width> <height>"
-}

