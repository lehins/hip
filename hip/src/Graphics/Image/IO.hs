{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.IO (
  -- $io
  -- ** Reading image files
  readImageGray, readImageRGB, readImageGrayA, readImageRGBA, readImageBinary,
  -- ** Writing or displaying images
  writeImage,
  IO.SaveOption(..), IO.OutputFormat(..), IO.Saveable(..),
  IO.setDisplayProgram,
  displayImage,
  -- ** Histograms
  displayImageHistograms, 
  H.Histogram,
  getHistograms,
  IO.displayHistograms,
  IO.writeHistograms
  ) where

import qualified Data.Vector.Unboxed as V
import Graphics.Image.Internal (Image, VectorStrategy(..), toVector)
import Graphics.Image.Pixel
import HIP.Conversion (toBinaryImage)
import qualified HIP.IO as IO
import qualified HIP.Histogram as H

-- $io HIP uses <http://hackage.haskell.org/package/JuicyPixels JuicyPixels> and
-- <http://hackage.haskell.org/package/netpbm Netpbm> to encode and decode
-- image files of different formats.

--------------------------------------------------------------------------------
---- IO ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Read any supported image file as a grayscale image.
readImageGray :: FilePath -> IO (Image Gray)
readImageGray = IO.readImage


-- | Same as 'readGrayImage', but reads it in with an alpha channel. If an image
-- file doesn't have an alpha channel, it will be added with opacity set to
-- @1.0@.
readImageGrayA :: FilePath -> IO (Image (Alpha Gray))
readImageGrayA = IO.readImage


-- | Read any supported image file as a color image in RGB colorspace.
readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB = IO.readImage


-- | Same as 'readGrayRGB', but reads it in with an alpha channel. If an image
-- file doesn't have an alpha channel, it will be added with opacity set to
-- @1.0@.
readImageRGBA :: FilePath -> IO (Image (Alpha RGB))
readImageRGBA = IO.readImage


-- | Read any supported image file as a grayscale image and convert it to Binary image.
readImageBinary :: FilePath -> IO (Image Binary)
readImageBinary = fmap toBinaryImage . readImageGray


-- | Write an image to file. This function will try it's best to guess an image
-- format it should be saved in by looking at the file extension.
writeImage :: (IO.Saveable Image px, Pixel px) =>
              [IO.SaveOption Image px]
           -> FilePath -- ^ Name of the file where image will be saved.
           -> Image px -- ^ Image to be saved.
           -- ^ A list of 'IO.SaveOptions' that will specify the output format
           -- colorspace etc. They are optional, pass an empty list to use
           -- default options.
           -> IO ()
writeImage = IO.writeImage Identity


-- | Display an image using an external program, which you can specify using
-- 'setDisplayProgram'. By default 'gpicview' program is used. This is function
-- writes an image into a temporary file in a system defined temporary folder
-- and passes that file to an external viewer. This is also a blocking function,
-- which means, running of the program will be suspended until external viewer
-- is closed.
--
--  >>> frog <- readImageRGB "images/frog.jpg"
--  >>> displayImage frog
--
displayImage :: (IO.Saveable Image px, Pixel px) =>
                Image px -- ^ Image to be displayed.
             -> IO ()
displayImage = IO.displayImage Identity


-- Histograms

-- | Display Histograms for each color channel of the image. Histograms plot is
-- displayed using an external program that can be changed with
-- 'IO.setDisplayProgram'.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> displayImageHistograms 255 frog
--
-- <<images/frog_histogram.png>>
--
displayImageHistograms :: (Pixel (Channel px), Pixel px, RealFrac (Channel px),
                           Enum (Channel px)) =>
                          Int -- ^ Number of intervals (bins) pixels should be
                              -- counted in.
                       -> Image px -- ^ Source Image.
                       -> IO ()
displayImageHistograms bins img = IO.displayHistograms $ getHistograms bins img


-- | Returns a list of Histograms, one 'H.Histogram' for each color channel of the
-- image.
getHistograms :: (Pixel px, Pixel (Channel px), RealFrac (Channel px)) =>
                 Int -- ^ Number of intervals (bins) pixels should be counted in.
              -> Image px -- ^ Source Image.
              -> [H.Histogram (Channel px)]
getHistograms bins img = H.getHistogramsUsing bins img maker where
  delta = 1 / fromIntegral bins
  -- here we use Unboxed Vectors to generate histograms, instead of Boxed as in
  -- default implementation
  maker = V.toList . H.getHistogramVector (bins+1) delta . toVector

