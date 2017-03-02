{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.IO.Formats.JuicyPixels.Common
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats.JuicyPixels.Common (
  BMP(..),
  GIF(..), GIFA(..),
  JP.GifDelay, JP.GifLooping(..), JP.PaletteOptions(..), JP.PaletteCreationMethod(..),
  HDR(..),
  JPG(..),
  PNG(..),
  TGA(..),
  TIF(..),
  SaveOption(..),
  ) where

import Graphics.Image.ColorSpace
import Graphics.Image.IO.Base
import qualified Codec.Picture as JP





-- | Bitmap image with @.bmp@ extension.
data BMP = BMP deriving Show

instance ImageFormat BMP where
  data SaveOption BMP

  ext _ = ".bmp"


-- | Graphics Interchange Format image with @.gif@ extension.
data GIF = GIF deriving Show

instance ImageFormat GIF where
  data SaveOption GIF = GIFPalette JP.PaletteOptions

  ext _ = ".gif"


  -- | Graphics Interchange Format animated image with @.gif@ extension.
data GIFA = GIFA deriving Show

instance ImageFormat GIFA where
  data SaveOption GIFA = GIFAPalette JP.PaletteOptions
                       | GIFALooping JP.GifLooping

  ext _ = ext GIF

-- | High-dynamic-range image with @.hdr@ or @.pic@ extension.
data HDR = HDR deriving Show

instance ImageFormat HDR where
  data SaveOption HDR

  ext _ = ".hdr"

  exts _ = [".hdr", ".pic"]


-- | Joint Photographic Experts Group image with @.jpg@ or @.jpeg@ extension.
data JPG = JPG deriving Show

instance ImageFormat JPG where
  data SaveOption JPG = JPGQuality Word8

  ext _ = ".jpg"

  exts _ = [".jpg", ".jpeg"]


-- | Portable Network Graphics image with @.png@ extension.
data PNG = PNG deriving Show

instance ImageFormat PNG where
  data SaveOption PNG

  ext _ = ".png"


-- | Truevision Graphics Adapter image with .tga extension.
data TGA = TGA

instance ImageFormat TGA where
  data SaveOption TGA

  ext _ = ".tga"


-- | Tagged Image File Format image with @.tif@ or @.tiff@ extension.
data TIF = TIF deriving Show

instance ImageFormat TIF where
  data SaveOption TIF

  ext _ = ".tif"

  exts _ = [".tif", ".tiff"]
