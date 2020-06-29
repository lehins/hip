{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.IO
-- Copyright   : (c) Alexey Kuleshevich 2016-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO
  (  -- * Reading
    readImage
  , readImageAuto
  , readImageY
  , readImageY8
  , readImageYA
  , readImageRGB
  , readImageRGB8
  , readImageRGBA
  -- * Writing
  , DefSpace
  , toDefSpace
  , writeImage
  , writeImageAuto
  -- TODO: reexport bunch of massiv-io stuff
  -- * Displaying
  , A.ExternalViewer(..)
  , displayImage
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

import Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as A
import Graphics.Image.Internal
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Unsafe.Coerce

-- | Display an image by writing it as a .tiff file to a temporary directory and making a call to an
-- external viewer that is set as a default image viewer by the OS.
-- displayImage ::
--      (MonadIO m, ColorModel cs e, A.Writable (A.Auto A.TIF) (Image cs e))
--   => Image cs e
--   -> m ()
displayImage ::
     ( ColorSpace (DefSpace cs) i e
     , ColorSpace (BaseSpace (DefSpace cs)) i e
     , MonadIO m
     )
  => Image cs e
  -> m ()
displayImage img = A.displayImage (unImage (toDefSpace img))
{-# NOINLINE displayImage #-}

-- | Maps basic color models to a default color space
type family DefSpace cs where
  DefSpace CM.Y = Y D65
  DefSpace CM.RGB = SRGB 'Linear
  DefSpace CM.HSI = HSI (SRGB 'NonLinear)
  DefSpace CM.HSV = HSV (SRGB 'NonLinear)
  DefSpace CM.HSL = HSL (SRGB 'NonLinear)
  DefSpace CM.CMYK = CMYK (SRGB 'Linear)
  DefSpace CM.YCbCr = YCbCr (SRGB 'NonLinear)
  DefSpace cs = cs

-- | Cast an image to a default color space
toDefSpace :: Image cs e -> Image (DefSpace cs) e
toDefSpace = unsafeCoerce

-- | Display an image by making a call to an external viewer that is set as a default image viewer
-- by the OS.
displayImageUsing ::
     ( MonadIO m
     , ColorSpace (DefSpace cs) i e
     , ColorSpace (BaseSpace (DefSpace cs)) i e
     )
  => A.ExternalViewer
  -> Bool
  -> Image cs e
  -> m ()
displayImageUsing ev block img = A.displayImageUsing ev block (unImage (toDefSpace img))
{-# NOINLINE displayImageUsing #-}


-- | Try to guess an image format from file's extension, then attempt to decode it as such. Color
-- space and precision of the result array must match exactly that of the actual image, in order to
-- apply auto conversion use `readImageAuto` instead.
--
-- Might throw `A.ConvertError`, `A.DecodeError`, besides other standard errors related to file IO.
--
-- Resulting image will be read as specified by the type signature:
--
-- >>> frog <- readImage "images/frog.jpg" :: IO (Image YCbCr Word8)
-- >>> displayImage frog
readImage ::
     (ColorModel cs e, MonadIO m)
  => FilePath -- ^ File path for an image
  -> m (Image cs e)
readImage path = Image <$> A.readImage path
{-# NOINLINE readImage #-}


-- | Same as `readImage`, but will perform any possible color space and
-- precision conversions in order to match the result image type. Very useful
-- whenever image format isn't known at compile time.
readImageAuto ::
     (ColorSpace cs i e, MonadIO m)
  => FilePath -- ^ File path for an image
  -> m (Image cs e)
readImageAuto path = Image <$> A.readImageAuto path
{-# INLINE readImageAuto #-}


-- | Read an image file and convert it to linear luminance (brightness), i.e. grayscale.
readImageY :: MonadIO m => FilePath -> m (Image CM.Y Double)
readImageY fp = do
  arr :: A.Image A.S (Y D65) Double <- A.readImageAuto fp
  pure $ Image $ A.toImageBaseModel arr
{-# INLINE readImageY #-}


-- | Read an image file and convert it to linear luminance (brightness), i.e. grayscale.
readImageY8 :: MonadIO m => FilePath -> m (Image CM.Y Word8)
readImageY8 fp = do
  arr :: A.Image A.S (Y D65) Word8 <- A.readImageAuto fp
  pure $ Image $ A.toImageBaseModel arr
{-# INLINE readImageY8 #-}


-- | Read an image file and convert it to linear luminance with 'Alpha' channel.
readImageYA :: MonadIO m => FilePath -> m (Image (Alpha CM.Y) Double)
readImageYA fp = do
  arr :: A.Image A.S (Alpha (Y D65)) Double <- A.readImageAuto fp
  pure $ Image $ A.toImageBaseModel arr
{-# INLINE readImageYA #-}


-- | Read an image and convert it into linear sRGB colorspace.
readImageRGB :: MonadIO m => FilePath -> m (Image CM.RGB Double)
readImageRGB fp = do
  arr :: A.Image A.S (SRGB 'Linear) Double <- A.readImageAuto fp
  pure $ Image $ A.toImageBaseModel arr
{-# INLINE readImageRGB #-}

-- | Read image in sRGB colorspace.
readImageRGB8 :: MonadIO m => FilePath -> m (Image CM.RGB Word8)
readImageRGB8 fp = do
  arr :: A.Image A.S (SRGB 'Linear) Word8 <- A.readImageAuto fp
  pure $ Image $ A.toImageBaseModel arr
{-# INLINE readImageRGB8 #-}


-- | Read image in sRGB colorspace with 'Alpha' channel.
readImageRGBA :: MonadIO m => FilePath -> m (Image (Alpha CM.RGB) Double)
readImageRGBA fp = do
  arr :: A.Image A.S (Alpha (SRGB 'Linear)) Double <- A.readImageAuto fp
  pure $ Image $ A.toImageBaseModel arr
{-# INLINE readImageRGBA #-}




-- | Inverse of the 'readImage', but similarly to it, will guess an output file format from the file
-- extension and will write to file any image with the colorspace that is supported by that
-- format. Precision of the image might be adjusted using `Elevator` whenever precision of the
-- source image is not supported by the image file format. For instance, @`Image` `RGBA` `Double`@
-- being saved as 'PNG' file would be written as @`Image` `RGBA` `Word16`@, thus using highest
-- supported precision `Word16` for that format. If automatic colors space conversion is also
-- desired, `writeImageAuto` can be used instead.
--
-- Can throw `A.ConvertError`, `A.EncodeError` and other usual IO errors.
--
writeImage ::
     (ColorModel cs e, MonadIO m)
  => FilePath
  -> Image cs e
  -> m ()
writeImage path (Image arr) = A.writeImage path arr
{-# INLINE writeImage #-}


-- | Write any image while doing any necessary color space and precision conversions.
writeImageAuto ::
     (ColorSpace (DefSpace cs) i e, ColorSpace (BaseSpace (DefSpace cs)) i e, MonadIO m)
  => FilePath
  -> Image cs e
  -> m ()
writeImageAuto path img = A.writeImageAuto path (unImage (toDefSpace img))
{-# INLINE writeImageAuto #-}

