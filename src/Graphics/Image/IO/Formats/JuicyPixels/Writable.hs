{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.IO.Formats.JuicyPixels.Writable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats.JuicyPixels.Writable () where

import Prelude as P
import qualified Data.Monoid as M (mempty)
import Graphics.Image.ColorSpace
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector
import Graphics.Image.IO.Base
import Graphics.Image.IO.Formats.JuicyPixels.Common
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Jpg as JP
import qualified Codec.Picture.ColorQuant as JP
import qualified Data.Vector.Storable as V

--------------------------------------------------------------------------------
-- Encoding images using JuicyPixels -------------------------------------------
--------------------------------------------------------------------------------

instance Writable (Image VS Y Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.Pixel8) id

instance Writable (Image VS RGB Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.PixelRGB8) id

instance Writable (Image VS RGBA Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.PixelRGBA8) id

instance Writable (Image VS Binary Bit) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.Pixel8) fromPixelBinary

instance Writable (Image VS Y Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.Pixel8) (fmap toWord8)

instance Writable (Image VS YA Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.Pixel8)
                                                ((fmap toWord8) . dropAlpha)

instance Writable (Image VS RGB Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.PixelRGB8) (fmap toWord8)

instance Writable (Image VS RGBA Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (undefined :: JP.PixelRGBA8) (fmap toWord8)

-- Writable GIF

encodeGIF :: (Array VS cs' e, Array VS cs Word8) =>
             [SaveOption GIF] -> (Pixel cs' e -> Pixel cs Word8)
             -> Image VS cs' e -> BL.ByteString
encodeGIF []                     !conv =
  either error id . uncurry JP.encodeGifImageWithPalette .
  JP.palettize JP.defaultPaletteOptions . imageToJPImage (undefined :: JP.PixelRGB8) conv
encodeGIF (GIFPalette palOpts:_) !conv =
  either error id . uncurry JP.encodeGifImageWithPalette .
  JP.palettize palOpts . imageToJPImage (undefined :: JP.PixelRGB8) conv


instance Writable (Image VS RGB Word8) GIF where
  encode _ opts = encodeGIF opts id
  
instance Writable (Image VS Y Double) GIF where
  encode _ opts = encodeGIF opts ((fmap toWord8) . toPixelRGB)
    
instance Writable (Image VS YA Double) GIF where
  encode _ opts = encodeGIF opts ((fmap toWord8) . toPixelRGB . dropAlpha)

instance Writable (Image VS RGB Double) GIF where
  encode _ opts = encodeGIF opts (fmap toWord8)

instance Writable (Image VS RGBA Double) GIF where
  encode _ opts = encodeGIF opts ((fmap toWord8) . dropAlpha)


encodeGIFA :: (Array VS cs' e, Array VS cs Word8) =>
              [SaveOption GIFA] -> (Pixel cs' e -> Pixel cs Word8)
           -> [(JP.GifDelay, Image VS cs' e)] -> BL.ByteString
encodeGIFA !opts !conv =
  either error id . JP.encodeGifImages (getGIFALoop opts) . P.map palletizeGif where
    getGIFALoop []                   = JP.LoopingNever
    getGIFALoop (GIFALooping loop:_) = loop
    getGIFALoop (_:xs)               = getGIFALoop xs    
    getGIFAPal []                      = JP.defaultPaletteOptions
    getGIFAPal (GIFAPalette palOpts:_) = palOpts
    getGIFAPal (_:xs)                  = getGIFAPal xs
    palletizeGif !(d, img) = (p, d, jimg) where  
      !(jimg, p) = JP.palettize (getGIFAPal opts) $
                   imageToJPImage (undefined :: JP.PixelRGB8) conv img


instance Writable [(JP.GifDelay, Image VS RGB Word8)] GIFA where
  encode _ opts = encodeGIFA opts id

instance Writable [(JP.GifDelay, Image VS RGB Double)] GIFA where
  encode _ opts = encodeGIFA opts (fmap toWord8)

-- Writable HDR

instance Writable (Image VS RGB Float) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage (undefined :: JP.PixelRGBF) id

instance Writable (Image VS Y Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage (undefined :: JP.PixelRGBF)
                                             (fmap toFloat . toPixelRGB)

instance Writable (Image VS YA Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage (undefined :: JP.PixelRGBF)
                                             (fmap toFloat . toPixelRGB . dropAlpha)

instance Writable (Image VS RGB Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage (undefined :: JP.PixelRGBF) (fmap toFloat)

instance Writable (Image VS RGBA Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage (undefined :: JP.PixelRGBF)
                                             (fmap toFloat . dropAlpha)
 

-- Writable JPG


encodeJPG
  :: (JP.JpgEncodable px, Array VS cs' e, Array VS cs (JP.PixelBaseComponent px)) =>
     [SaveOption JPG]
     -> px
     -> (Pixel cs' e -> Pixel cs (JP.PixelBaseComponent px))
     -> Image VS cs' e
     -> BL.ByteString
encodeJPG []               t conv =
  JP.encodeDirectJpegAtQualityWithMetadata 100 M.mempty . imageToJPImage t conv
encodeJPG (JPGQuality q:_) t conv =
  JP.encodeDirectJpegAtQualityWithMetadata q M.mempty . imageToJPImage t conv


instance Writable (Image VS Y Word8) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.Pixel8) id

instance Writable (Image VS RGB Word8) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.PixelRGB8) id

instance Writable (Image VS CMYK Word8) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.PixelCMYK8) id
               
instance Writable (Image VS YCbCr Word8) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.PixelYCbCr8) id

instance Writable (Image VS Y Double) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.Pixel8) (fmap toWord8)

instance Writable (Image VS YA Double) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.Pixel8) ((fmap toWord8) . dropAlpha) 

instance Writable (Image VS RGB Double) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.PixelRGB8) (fmap toWord8)

instance Writable (Image VS RGBA Double) JPG where
  encode _ opts = encodeJPG opts (undefined :: JP.PixelRGB8) ((fmap toWord8) . dropAlpha) 


-- Writable PNG

instance Writable (Image VS Binary Bit) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.Pixel8) fromPixelBinary
  
instance Writable (Image VS Y Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.Pixel8) id

instance Writable (Image VS Y Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.Pixel16) id

instance Writable (Image VS YA Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelYA8) id

instance Writable (Image VS YA Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelYA16) id

instance Writable (Image VS RGB Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelRGB8) id

instance Writable (Image VS RGB Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelRGB16) id

instance Writable (Image VS RGBA Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelRGBA8) id

instance Writable (Image VS RGBA Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelRGBA16) id


instance Writable (Image VS Y Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.Pixel16) (fmap toWord16)

instance Writable (Image VS YA Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelYA16) (fmap toWord16)

instance Writable (Image VS RGB Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelRGB16) (fmap toWord16)

instance Writable (Image VS RGBA Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (undefined :: JP.PixelRGBA16) (fmap toWord16)

-- Writable TGA

instance Writable (Image VS Binary Bit) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.Pixel8) fromPixelBinary
  
instance Writable (Image VS Y Word8) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.Pixel8) id

instance Writable (Image VS RGB Word8) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.PixelRGB8) id

instance Writable (Image VS RGBA Word8) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.PixelRGBA8) id


instance Writable (Image VS Y Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.Pixel8) (fmap toWord8)

instance Writable (Image VS YA Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.Pixel8) ((fmap toWord8) . dropAlpha)

instance Writable (Image VS RGB Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.PixelRGB8) (fmap toWord8)

instance Writable (Image VS RGBA Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (undefined :: JP.PixelRGBA8) (fmap toWord8)

-- Writable TIF

instance Writable (Image VS Y Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.Pixel8) id

instance Writable (Image VS Y Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.Pixel16) id

instance Writable (Image VS YA Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelYA8) id

instance Writable (Image VS YA Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelYA16) id

instance Writable (Image VS RGB Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelRGB8) id

instance Writable (Image VS RGB Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelRGB16) id

instance Writable (Image VS RGBA Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelRGBA8) id

instance Writable (Image VS RGBA Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelRGBA16) id

instance Writable (Image VS YCbCr Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelYCbCr8) id
  
instance Writable (Image VS CMYK Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelCMYK8) id

instance Writable (Image VS CMYK Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelCMYK16) id


instance Writable (Image VS Binary Bit) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.Pixel8) fromPixelBinary
  
instance Writable (Image VS Y Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.Pixel16) (fmap toWord16)

instance Writable (Image VS YA Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelYA16) (fmap toWord16)

instance Writable (Image VS RGB Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelRGB16) (fmap toWord16)

instance Writable (Image VS RGBA Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelRGBA16) (fmap toWord16)

instance Writable (Image VS YCbCr Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelYCbCr8) (fmap toWord8)

instance Writable (Image VS CMYK Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (undefined :: JP.PixelCMYK16) (fmap toWord16)



imageToJPImage :: (JP.Pixel a, Array VS cs' e, Array VS cs (JP.PixelBaseComponent a)) =>
                  a -> (Pixel cs' e -> Pixel cs (JP.PixelBaseComponent a)) -> Image VS cs' e -> JP.Image a
imageToJPImage _ f !img = JP.Image n m $ V.unsafeCast $ toVector $ I.map f img where
  !(m, n) = dims img
