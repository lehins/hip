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
  GIF(..), JP.GifDelay, JP.GifLooping(..), JP.PaletteOptions(..), JP.PaletteCreationMethod(..),
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
data BMP = BMP

instance ImageFormat BMP where
  data SaveOption BMP

  ext _ = ".bmp"


-- | Graphics Interchange Format image with @.gif@ extension.
data GIF = GIF

instance ImageFormat GIF where
  data SaveOption GIF = GIFPalette JP.PaletteOptions
  
  ext _ = ".gif"

  
-- TODO: Create (Seq GIF)
instance ImageFormat [GIF] where
  data SaveOption [GIF] = GIFsPalette JP.PaletteOptions
                        | GIFsLooping JP.GifLooping

  ext _ = ext GIF

-- | High-dynamic-range image with @.hdr@ or @.pic@ extension.
data HDR = HDR

instance ImageFormat HDR where
  data SaveOption HDR

  ext _ = ".hdr"

  exts _ = [".hdr", ".pic"]


-- | Joint Photographic Experts Group image with @.jpg@ or @.jpeg@ extension.
data JPG = JPG

instance ImageFormat JPG where
  data SaveOption JPG = JPGQuality Word8

  ext _ = ".jpg"

  exts _ = [".jpg", ".jpeg"]


-- | Portable Network Graphics image with @.png@ extension.
data PNG = PNG

instance ImageFormat PNG where
  data SaveOption PNG

  ext _ = ".png"


-- | Truevision Graphics Adapter image with .tga extension.
data TGA = TGA

instance ImageFormat TGA where
  data SaveOption TGA

  ext _ = ".tga"


-- | Tagged Image File Format image with @.tif@ or @.tiff@ extension.
data TIF = TIF

instance ImageFormat TIF where
  data SaveOption TIF  

  ext _ = ".tif"

  exts _ = [".tif", ".tiff"]
