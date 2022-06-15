{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Graphics.Image.IO
-- Copyright   : (c) Alexey Kuleshevich 2016-2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO
  (  -- * Reading
    readImage
  , readImageY
  , readImageYA
  , readImageY'
  , readImageY'A
  , readImageRGB
  , readImageRGBA
  , readImageBinary
  , readImageExact
  -- * Writing
  , writeImage
  , writeImageExact
  -- * Displaying
  , A.ExternalViewer(..)
  , displayImage
  , displayImageExact
  , displayImageUsing
  -- ** Common viewers
  , A.displayImageFile
  , A.defaultViewer
  , A.eogViewer
  , A.gpicviewViewer
  , A.fehViewer
  , A.gimpViewer
  -- * Supported Image Formats
  , A.Auto(..)
  -- ** BMP
  , A.BMP(..)
  -- ** GIF
  , A.GIF(..)
  , A.GifOptions(..)
  , A.SequenceGifOptions(..)
  , A.GifDelay
  , A.GifLooping(..)
  , A.PaletteOptions(..)
  , A.PaletteCreationMethod(..)
  , A.GifDisposalMethod(..)
  -- ** HDR
  , A.HDR(..)
  -- ** JPG
  , A.JPG(..)
  , A.JpegOptions(..)
  -- ** PNG
  , A.PNG(..)
  -- ** TGA
  , A.TGA(..)
  -- ** TIF
  , A.TIF(..)
  -- ** PBM
  , A.PBM(..)
  -- ** PGM
  , A.PGM(..)
  -- ** PPM
  , A.PPM(..)
  -- $supported
  -- * Hands on examples
  -- ** Animated GIF
  -- $animation
  ) where

import Prelude hiding (map)
import qualified Data.Massiv.Array.IO as A
import Graphics.Image.Internal as I
import Graphics.Image.Processing.Binary
import Control.Exception (try)

-- | Display an image by writing it as a .tiff file to a temporary directory and making a call to an
-- external viewer that is set as a default image viewer by the OS.
-- displayImage ::
--      (MonadIO m, ColorModel cs e, A.Writable (A.Auto A.TIF) (Image cs e))
--   => Image cs e
--   -> m ()
displayImage ::
     ( ColorSpace cs i e
     , ColorSpace (BaseSpace cs) i e
     , MonadIO m
     )
  => Image cs e
  -> m ()
displayImage = A.displayImage . unImage
{-# NOINLINE displayImage #-}

displayImageExact :: (ColorModel cs e, MonadIO m) => Image cs e -> m ()
displayImageExact (Image img) =
  let go [] excs = putStrLn $ unlines ("Tried all these encoders and failed:" : reverse excs)
      go (f:fs) excs =
        try (A.displayImageUsingAdhoc A.defaultViewer False f img) >>= \case
          Left (exc :: A.ConvertError) -> go fs (displayException exc : excs)
          Right () -> pure ()
   in liftIO $ go A.imageWriteFormats []
{-# NOINLINE displayImageExact #-}

-- | Display an image by making a call to an external viewer that is set as a default image viewer
-- by the OS.
displayImageUsing ::
     ( MonadIO m
     , ColorSpace cs i e
     , ColorSpace (BaseSpace cs) i e
     )
  => A.ExternalViewer
  -> Bool
  -> Image cs e
  -> m ()
displayImageUsing ev block = A.displayImageUsing ev block . unImage
{-# NOINLINE displayImageUsing #-}





-- | Try to guess an image format from file's extension, then attempt to decode it as such. Color
-- space and precision of the result image must match exactly that of the actual image, in order to
-- apply auto conversion use `readImageAuto` instead.
--
-- Might throw `A.ConvertError`, `A.DecodeError`, besides other standard errors related to file IO.
--
-- Resulting image will be read as specified by the type signature:
--
-- >>> frog <- readImageExact "images/frog.jpg" :: IO (Image YCbCr Word8)
-- >>> displayImage frog
readImageExact ::
     (ColorModel cs e, MonadIO m)
  => FilePath -- ^ File path for an image
  -> m (Image cs e)
readImageExact = fmap Image . A.readImage
{-# NOINLINE readImageExact #-}


-- | Same as `readImage`, but will perform all possible color space and
-- precision conversions in order to match the result image type. Very useful
-- whenever image format isn't known at compile time.
readImage ::
     (ColorSpace cs i e, MonadIO m)
  => FilePath -- ^ File path for an image
  -> m (Image cs e)
readImage = fmap Image . A.readImageAuto
{-# INLINEABLE readImage #-}


-- | Read an image file and convert it to non-linear grayscale, i.e. Luma
readImageY' :: MonadIO m => FilePath -> m (Image (Y' SRGB) Double)
readImageY' = fmap Image . A.readImageAuto
  -- TODO: Optimize for YCbCr: (better in massiv-io)
  -- try decodeM as YCbCr, on decode error fallback to SRGB
{-# INLINEABLE readImageY' #-}

-- | Read an image file and convert it to non-linear grayscale, i.e. Luma
readImageY'A :: MonadIO m => FilePath -> m (Image (Alpha (Y' SRGB)) Double)
readImageY'A = fmap Image . A.readImageAuto
{-# INLINEABLE readImageY'A #-}

-- | Read an image file and convert it to linear luminance (brightness), i.e. grayscale.
readImageY :: MonadIO m => FilePath -> m (Image (Y D65) Double)
readImageY = fmap Image . A.readImageAuto
{-# INLINEABLE readImageY #-}


-- | Read an image file and convert it to linear luminance with 'Alpha' channel.
readImageYA :: MonadIO m => FilePath -> m (Image (Alpha (Y D65)) Double)
readImageYA = fmap Image . A.readImageAuto
{-# INLINEABLE readImageYA #-}


-- | Read an image and convert it into linear sRGB colorspace with Double precision
readImageRGB :: MonadIO m => FilePath -> m (Image (SRGB 'Linear) Double)
readImageRGB = fmap Image . A.readImageAuto
{-# INLINEABLE readImageRGB #-}


-- | Read image in sRGB colorspace with 'Alpha' channel.
readImageRGBA :: MonadIO m => FilePath -> m (Image (Alpha (SRGB 'Linear)) Double)
readImageRGBA = fmap Image . A.readImageAuto
{-# INLINEABLE readImageRGBA #-}


-- | Read any grayscale image file as binary by thresholding all values in the middle. See
-- `toImageBinary` for more info on thresholding.
readImageBinary :: MonadIO m => FilePath -> m (Image X Bit)
readImageBinary = fmap (toImageBinary @Word8 . Image) . A.readImage
{-# INLINEABLE readImageBinary #-}


-- | Write an image to a file. It will guess an output file format from the file extension
-- and will attempt to encode it in the colorspace that was supplied, but will throw an
-- exception if it is not supported by the guessed format. Unless you are looking to
-- specify the color space and precision for the file exactly, it is better to use
-- `writeImage`, which will perform the necessary conversions for you.
--
-- Can throw `A.ConvertError`, `A.EncodeError` and other usual IO errors.
--
writeImageExact ::
     (ColorModel cs e, MonadIO m)
  => FilePath
  -> Image cs e
  -> m ()
writeImageExact path = A.writeImage path . unImage
{-# INLINEABLE writeImageExact #-}


-- | Write any image while doing any necessary color space and precision conversions.
writeImage ::
     (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, MonadIO m) => FilePath -> Image cs e -> m ()
writeImage path = A.writeImageAuto path . unImage
{-# INLINEABLE writeImage #-}
