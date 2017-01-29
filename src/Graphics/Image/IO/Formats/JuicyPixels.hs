{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.IO.Formats.JuicyPixels
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats.JuicyPixels (
  BMP(..),
  GIF(..), JP.GifDelay, JP.GifLooping(..), JP.PaletteOptions(..), JP.PaletteCreationMethod(..),
  HDR(..),
  JPG(..),
  PNG(..),
  TGA(..),
  TIF(..),
  SaveOption(..),
  ) where

import Prelude as P
import GHC.Float
import Data.Either
import qualified Data.Monoid as M (mempty)
import Graphics.Image.ColorSpace
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector
import Graphics.Image.IO.Base
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Jpg as JP
import qualified Codec.Picture.Types as JP
import qualified Codec.Picture.ColorQuant as JP
import qualified Data.Vector.Storable as V

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


--------------------------------------------------------------------------------
-- Converting to and from JuicyPixels ------------------------------------------
--------------------------------------------------------------------------------

-- Y -> Y (Double)

instance Convertible JP.Pixel8 (Pixel Y Double) where
  convert = fmap toDouble . PixelY

instance Convertible JP.Pixel16 (Pixel Y Double) where
  convert = fmap toDouble . PixelY

instance Convertible JP.PixelF (Pixel Y Double) where
  convert = fmap toDouble . PixelY

instance Convertible JP.PixelYA8 (Pixel Y Double) where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelYA16 (Pixel Y Double) where
  convert = convert . JP.dropTransparency

instance Convertible JP.Pixel8 (Pixel YA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.Pixel16 (Pixel YA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelF (Pixel YA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelYA8 (Pixel YA Double) where
  convert (JP.PixelYA8 y a) = fmap toDouble (PixelYA y a)

instance Convertible JP.PixelYA16 (Pixel YA Double) where
  convert (JP.PixelYA16 y a) = fmap toDouble (PixelYA y a)


-- Color -> Y (Double)

instance Convertible JP.PixelRGB8 (Pixel Y Double) where
  convert = toPixelY . (convert :: JP.PixelRGB8 -> Pixel RGB Double)

instance Convertible JP.PixelRGB16 (Pixel Y Double) where
  convert = toPixelY . (convert :: JP.PixelRGB16 -> Pixel RGB Double)

instance Convertible JP.PixelRGBA8 (Pixel Y Double) where
  convert = toPixelY . (convert :: JP.PixelRGBA8 -> Pixel RGB Double)

instance Convertible JP.PixelRGBA16 (Pixel Y Double) where
  convert = toPixelY . (convert :: JP.PixelRGBA16 -> Pixel RGB Double)

instance Convertible JP.PixelRGBF (Pixel Y Double) where
  convert = toPixelY . (convert :: JP.PixelRGBF -> Pixel RGB Double)

instance Convertible JP.PixelCMYK8 (Pixel Y Double) where
  convert = toPixelY . fmap toDouble . (convert :: JP.PixelCMYK8 -> Pixel CMYK Word8)

instance Convertible JP.PixelCMYK16 (Pixel Y Double) where
  convert = toPixelY . fmap toDouble . (convert :: JP.PixelCMYK16 -> Pixel CMYK Word16)

instance Convertible JP.PixelYCbCr8 (Pixel Y Double) where
  convert = convert . JP.computeLuma

-- Color -> YA (Double)  

instance Convertible JP.PixelRGB8 (Pixel YA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelRGB16 (Pixel YA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelRGBF (Pixel YA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelCMYK8 (Pixel YA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelCMYK16 (Pixel YA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelYCbCr8 (Pixel YA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelRGBA8 (Pixel YA Double) where
  convert = toPixelYA . (convert :: JP.PixelRGBA8 -> Pixel RGBA Double)

instance Convertible JP.PixelRGBA16 (Pixel YA Double) where
  convert = toPixelYA . (convert :: JP.PixelRGBA16 -> Pixel RGBA Double)
  
  

-- Y -> RGB (Double)

instance Convertible JP.Pixel8 (Pixel RGB Double) where
  convert = promote . toDouble

instance Convertible JP.Pixel16 (Pixel RGB Double) where
  convert = promote . toDouble

instance Convertible JP.PixelF (Pixel RGB Double) where
  convert = promote . toDouble

instance Convertible JP.PixelYA8 (Pixel RGB Double) where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelYA16 (Pixel RGB Double) where
  convert = convert . JP.dropTransparency

-- Color -> RGB (Double)

instance Convertible JP.PixelRGB8 (Pixel RGB Double) where
  convert (JP.PixelRGB8 r g b) = fmap toDouble $ PixelRGB r g b

instance Convertible JP.PixelRGB16 (Pixel RGB Double) where
  convert (JP.PixelRGB16 r g b) = fmap toDouble $ PixelRGB r g b

instance Convertible JP.PixelRGBA8 (Pixel RGB Double) where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelRGBA16 (Pixel RGB Double) where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelRGBF (Pixel RGB Double) where
  convert (JP.PixelRGBF r g b) = 
    PixelRGB (float2Double r) (float2Double g) (float2Double b)

instance Convertible JP.PixelYCbCr8 (Pixel RGB Double) where
  convert = convert . (JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8)

instance Convertible JP.PixelCMYK8 (Pixel RGB Double) where
  convert = convert . (JP.convertPixel :: JP.PixelCMYK8 -> JP.PixelRGB8)

instance Convertible JP.PixelCMYK16 (Pixel RGB Double) where
  convert = convert . (JP.convertPixel :: JP.PixelCMYK16 -> JP.PixelRGB16)

-- Y -> RGBA (Double)

instance Convertible JP.Pixel8 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.Pixel16 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelF (Pixel RGBA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelYA8 (Pixel RGBA Double) where
  convert = toPixelRGBA . (convert :: JP.PixelYA8 -> Pixel YA Double)

instance Convertible JP.PixelYA16 (Pixel RGBA Double) where
  convert = toPixelRGBA . (convert :: JP.PixelYA16 -> Pixel YA Double)

-- Color -> RGBA (Double)

instance Convertible JP.PixelRGB8 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelRGB16 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelRGBF (Pixel RGBA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelCMYK8 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelCMYK16 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert

instance Convertible JP.PixelYCbCr8 (Pixel RGBA Double) where
  convert = addAlpha 1 . convert
  
instance Convertible JP.PixelRGBA8 (Pixel RGBA Double) where
  convert (JP.PixelRGBA8 r g b a) = fmap toDouble $ PixelRGBA r g b a
  
instance Convertible JP.PixelRGBA16 (Pixel RGBA Double) where
  convert (JP.PixelRGBA16 r g b a) = fmap toDouble $ PixelRGBA r g b a


---- to JuicyPixels -----

---- Exact precision conversions

instance Convertible JP.Pixel8 (Pixel Y Word8) where
  convert = PixelY
  
instance Convertible JP.Pixel16 (Pixel Y Word16) where
  convert = PixelY

instance Convertible JP.Pixel32 (Pixel Y Word32) where
  convert = PixelY

instance Convertible JP.PixelF (Pixel Y Float) where
  convert = PixelY

instance Convertible JP.PixelYA8 (Pixel YA Word8) where
  convert (JP.PixelYA8 g a) = PixelYA g a
  
instance Convertible JP.PixelYA16 (Pixel YA Word16) where
  convert (JP.PixelYA16 g a) = PixelYA g a

instance Convertible JP.PixelRGB8 (Pixel RGB Word8) where
  convert (JP.PixelRGB8 r g b) = PixelRGB r g b
  
instance Convertible JP.PixelRGB16 (Pixel RGB Word16) where
  convert (JP.PixelRGB16 r g b) = PixelRGB r g b

instance Convertible JP.PixelRGBF (Pixel RGB Float) where
  convert (JP.PixelRGBF r g b) = PixelRGB r g b

instance Convertible JP.PixelRGBA8 (Pixel RGBA Word8) where
  convert (JP.PixelRGBA8 r g b a) = PixelRGBA r g b a
  
instance Convertible JP.PixelRGBA16 (Pixel RGBA Word16) where
  convert (JP.PixelRGBA16 r g b a) = PixelRGBA r g b a

instance Convertible JP.PixelYCbCr8 (Pixel YCbCr Word8) where
  convert (JP.PixelYCbCr8 y cb cr) = PixelYCbCr y cb cr

instance Convertible JP.PixelCMYK8 (Pixel CMYK Word8) where
  convert (JP.PixelCMYK8 c m y k) = PixelCMYK c m y k

instance Convertible JP.PixelCMYK16 (Pixel CMYK Word16) where
  convert (JP.PixelCMYK16 c m y k) = PixelCMYK c m y k



instance Convertible (Pixel Y Word8) JP.Pixel8 where
  convert (PixelY g) = g
  
instance Convertible (Pixel Y Word16) JP.Pixel16 where
  convert (PixelY g) = g

instance Convertible (Pixel Y Word32) JP.Pixel32 where
  convert (PixelY g) = g

instance Convertible (Pixel Y Float) JP.PixelF where
  convert (PixelY g) = g

instance Convertible (Pixel YA Word8) JP.PixelYA8 where
  convert (PixelYA g a) = JP.PixelYA8 g a
  
instance Convertible (Pixel YA Word16) JP.PixelYA16 where
  convert (PixelYA g a) = JP.PixelYA16 g a

instance Convertible (Pixel RGB Word8) JP.PixelRGB8 where
  convert (PixelRGB r g b) = JP.PixelRGB8 r g b
  
instance Convertible (Pixel RGB Word16) JP.PixelRGB16 where
  convert (PixelRGB r g b) = JP.PixelRGB16 r g b

instance Convertible (Pixel RGB Float) JP.PixelRGBF where
  convert (PixelRGB r g b) = JP.PixelRGBF r g b

instance Convertible (Pixel RGBA Word8) JP.PixelRGBA8 where
  convert (PixelRGBA r g b a) = JP.PixelRGBA8 r g b a
  
instance Convertible (Pixel RGBA Word16) JP.PixelRGBA16 where
  convert (PixelRGBA r g b a) = JP.PixelRGBA16 r g b a


instance Convertible (Pixel YCbCr Word8) JP.PixelYCbCr8 where
  convert (PixelYCbCr y cb cr) = JP.PixelYCbCr8 y cb cr

instance Convertible (Pixel CMYK Word8) JP.PixelCMYK8 where
  convert (PixelCMYK c m y k) = JP.PixelCMYK8 c m y k

instance Convertible (Pixel CMYK Word16) JP.PixelCMYK16 where
  convert (PixelCMYK c m y k) = JP.PixelCMYK16 c m y k



--------------------------------------------------------------------------------
-- Decoding images using JuicyPixels ------------------------------------------
--------------------------------------------------------------------------------


-- BMP Format Reading

instance Readable (Image VS Binary Bit) BMP where
  decode _ = fmap toImageBinary . jpImageY8ToImage . JP.decodeBitmap

instance Readable (Image VS Y Word8) BMP where
  decode _ = jpImageY8ToImage . JP.decodeBitmap

instance Readable (Image VS RGB Word8) BMP where
  decode _ = jpImageRGB8ToImage . JP.decodeBitmap

instance Readable (Image VS RGBA Word8) BMP where
  decode _ = jpImageRGBA8ToImage . JP.decodeBitmap



-- GIF Format Reading

instance Readable (Image VS RGB Word8) GIF where
  decode _ = jpImageRGB8ToImage . JP.decodeGif

instance Readable (Image VS RGBA Word8) GIF where
  decode _ = jpImageRGBA8ToImage . JP.decodeGif


-- List of GIF Format frames Reading

decodeGifs :: (Either String JP.DynamicImage -> Either String img)
           -> B.ByteString -> Either String [img]
decodeGifs decoder = either Left decodeLS . JP.decodeGifImages where
    decodeLS ls = if null errs then Right imgs else Left $ unlines errs where
      (errs, imgs) = partitionEithers $ P.map (decoder . Right) ls

instance Readable [Image VS RGB Word8] [GIF] where
  decode _ = decodeGifs jpImageRGB8ToImage

instance Readable [Image VS RGBA Word8] [GIF] where
  decode _ = decodeGifs jpImageRGBA8ToImage


-- HDR Format Reading

instance Readable (Image VS RGB Float) HDR where
  decode _ = jpImageRGBFToImage . JP.decodeHDR


-- JPG Format Reading

instance Readable (Image VS Y Word8) JPG where
  decode _ = jpImageY8ToImage . JP.decodeJpeg

instance Readable (Image VS YA Word8) JPG where
  decode _ = jpImageYA8ToImage . JP.decodeJpeg

instance Readable (Image VS RGB Word8) JPG where
  decode _ = jpImageRGB8ToImage . JP.decodeJpeg

instance Readable (Image VS CMYK Word8) JPG where
  decode _ = jpImageCMYK8ToImage . JP.decodeJpeg

instance Readable (Image VS YCbCr Word8) JPG where
  decode _ = jpImageYCbCr8ToImage . JP.decodeJpeg


-- PNG Format Reading

instance Readable (Image VS Binary Bit) PNG where
  decode _ = fmap toImageBinary . jpImageY8ToImage . JP.decodePng

instance Readable (Image VS Y Word8) PNG where
  decode _ = jpImageY8ToImage . JP.decodePng

instance Readable (Image VS Y Word16) PNG where
  decode _ = jpImageY16ToImage . JP.decodePng

instance Readable (Image VS YA Word8) PNG where
  decode _ = jpImageYA8ToImage . JP.decodePng

instance Readable (Image VS YA Word16) PNG where
  decode _ = jpImageYA16ToImage . JP.decodePng

instance Readable (Image VS RGB Word8) PNG where
  decode _ = jpImageRGB8ToImage . JP.decodePng

instance Readable (Image VS RGB Word16) PNG where
  decode _ = jpImageRGB16ToImage . JP.decodePng

instance Readable (Image VS RGBA Word8) PNG where
  decode _ = jpImageRGBA8ToImage . JP.decodePng

instance Readable (Image VS RGBA Word16) PNG where
  decode _ = jpImageRGBA16ToImage . JP.decodePng


-- TGA Format Reading

instance Readable (Image VS Binary Bit) TGA where
  decode _ = fmap toImageBinary . jpImageY8ToImage . JP.decodeTga

instance Readable (Image VS Y Word8) TGA where
  decode _ = jpImageY8ToImage . JP.decodeTga

instance Readable (Image VS RGB Word8) TGA where
  decode _ = jpImageRGB8ToImage . JP.decodeTga

instance Readable (Image VS RGBA Word8) TGA where
  decode _ = jpImageRGBA8ToImage . JP.decodeTga


-- TIF Format Reading

instance Readable (Image VS Binary Bit) TIF where
  decode _ = fmap toImageBinary . jpImageY8ToImage . JP.decodeTiff

instance Readable (Image VS Y Word8) TIF where
  decode _ = jpImageY8ToImage . JP.decodeTiff

instance Readable (Image VS Y Word16) TIF where
  decode _ = jpImageY16ToImage . JP.decodeTiff

instance Readable (Image VS YA Word8) TIF where
  decode _ = jpImageYA8ToImage . JP.decodeTiff

instance Readable (Image VS YA Word16) TIF where
  decode _ = jpImageYA16ToImage . JP.decodeTiff

instance Readable (Image VS RGB Word8) TIF where
  decode _ = jpImageRGB8ToImage . JP.decodeTiff

instance Readable (Image VS RGB Word16) TIF where
  decode _ = jpImageRGB16ToImage . JP.decodeTiff

instance Readable (Image VS RGBA Word8) TIF where
  decode _ = jpImageRGBA8ToImage . JP.decodeTiff

instance Readable (Image VS RGBA Word16) TIF where
  decode _ = jpImageRGBA16ToImage . JP.decodeTiff

instance Readable (Image VS CMYK Word8) TIF where
  decode _ = jpImageCMYK8ToImage . JP.decodeTiff

instance Readable (Image VS CMYK Word16) TIF where
  decode _ = jpImageCMYK16ToImage . JP.decodeTiff


-- To Double precision safe conversion

instance Array arr Y Double => Readable (Image arr Y Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Array arr YA Double => Readable (Image arr YA Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Array arr RGB Double => Readable (Image arr RGB Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Array arr RGBA Double => Readable (Image arr RGBA Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap


instance Array arr Y Double => Readable (Image arr Y Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Array arr YA Double => Readable (Image arr YA Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Array arr RGB Double => Readable (Image arr RGB Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Array arr RGBA Double => Readable (Image arr RGBA Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif


instance Array arr Y Double => Readable [Image arr Y Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage

instance Array arr YA Double => Readable [Image arr YA Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage

instance Array arr RGB Double => Readable [Image arr RGB Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage

instance Array arr RGBA Double => Readable [Image arr RGBA Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage


instance Array arr Y Double => Readable (Image arr Y Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Array arr YA Double => Readable (Image arr YA Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Array arr RGB Double => Readable (Image arr RGB Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Array arr RGBA Double => Readable (Image arr RGBA Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR


instance Array arr Y Double => Readable (Image arr Y Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Array arr YA Double => Readable (Image arr YA Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Array arr RGB Double => Readable (Image arr RGB Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Array arr RGBA Double => Readable (Image arr RGBA Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg


instance Array arr Y Double => Readable (Image arr Y Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Array arr YA Double => Readable (Image arr YA Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Array arr RGB Double => Readable (Image arr RGB Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Array arr RGBA Double => Readable (Image arr RGBA Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng


instance Array arr Y Double => Readable (Image arr Y Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Array arr YA Double => Readable (Image arr YA Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Array arr RGB Double => Readable (Image arr RGB Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Array arr RGBA Double => Readable (Image arr RGBA Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga


instance Array arr Y Double => Readable (Image arr Y Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Array arr YA Double => Readable (Image arr YA Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Array arr RGB Double => Readable (Image arr RGB Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Array arr RGBA Double => Readable (Image arr RGBA Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff



-- General decoding and helper functions

jpImageToImageUnsafe :: (Array VS cs e, JP.Pixel jpx) =>
                  JP.Image jpx -> Image VS cs e
jpImageToImageUnsafe (JP.Image n m v) = fromVector (m, n) $ V.unsafeCast v



jpImageToImageSafe :: (Array arr cs e, Convertible jpx (Pixel cs e), JP.Pixel jpx) =>
                      JP.Image jpx -> Image arr cs e
jpImageToImageSafe jimg = makeImage (JP.imageHeight jimg, JP.imageWidth jimg) getPx
  where getPx (y, x) = convert $ JP.pixelAt jimg x y


jpImageY8ToImage :: Either String JP.DynamicImage -> Either String (Image VS Y Word8)
jpImageY8ToImage (Right (JP.ImageY8 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageY8ToImage jimg = jpCSError "Y8 (Pixel Y Word8)" jimg


jpImageY16ToImage :: Either String JP.DynamicImage -> Either String (Image VS Y Word16)
jpImageY16ToImage (Right (JP.ImageY16 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageY16ToImage jimg = jpCSError "Y16 (Pixel Y Word16)" jimg

{- -- No JuicyPixels images are actually read in this type
jpImageYFToImage :: Either String JP.DynamicImage -> Either String (Image VS Y Float)
jpImageYFToImage (Right (JP.ImageYF jimg)) = Right (jpImageToImage jimg)
jpImageYFToImage jimg = jpCSError "YF (Pixel Y Float)" jimg
-}

jpImageYA8ToImage :: Either String JP.DynamicImage -> Either String (Image VS YA Word8)
jpImageYA8ToImage (Right (JP.ImageYA8 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageYA8ToImage jimg = jpCSError "YA8 (Pixel YA Word8)" jimg


jpImageYA16ToImage :: Either String JP.DynamicImage -> Either String (Image VS YA Word16)
jpImageYA16ToImage (Right (JP.ImageYA16 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageYA16ToImage jimg = jpCSError "YA16 (Pixel YA Word16)" jimg


jpImageRGB8ToImage :: Either String JP.DynamicImage -> Either String (Image VS RGB Word8)
jpImageRGB8ToImage (Right (JP.ImageRGB8 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageRGB8ToImage jimg = jpCSError "RGB8 (Pixel RGB Word8)" jimg


jpImageRGB16ToImage :: Either String JP.DynamicImage -> Either String (Image VS RGB Word16)
jpImageRGB16ToImage (Right (JP.ImageRGB16 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageRGB16ToImage jimg = jpCSError "RGB16 (Pixel RGB Word16)" jimg


jpImageRGBFToImage :: Either String JP.DynamicImage -> Either String (Image VS RGB Float)
jpImageRGBFToImage (Right (JP.ImageRGBF jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageRGBFToImage jimg = jpCSError "RGBF (Pixel RGB Float)" jimg


jpImageRGBA8ToImage :: Either String JP.DynamicImage -> Either String (Image VS RGBA Word8)
jpImageRGBA8ToImage (Right (JP.ImageRGBA8 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageRGBA8ToImage jimg = jpCSError "RGBA8 (Pixel RGBA Word8)" jimg


jpImageRGBA16ToImage :: Either String JP.DynamicImage -> Either String (Image VS RGBA Word16)
jpImageRGBA16ToImage (Right (JP.ImageRGBA16 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageRGBA16ToImage jimg = jpCSError "RGBA16 (Pixel RGBA Word16)" jimg


jpImageYCbCr8ToImage :: Either String JP.DynamicImage -> Either String (Image VS YCbCr Word8)
jpImageYCbCr8ToImage (Right (JP.ImageYCbCr8 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageYCbCr8ToImage jimg = jpCSError "YCbCr8 (Pixel YCbCr Word8)" jimg


jpImageCMYK8ToImage :: Either String JP.DynamicImage -> Either String (Image VS CMYK Word8)
jpImageCMYK8ToImage (Right (JP.ImageCMYK8 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageCMYK8ToImage jimg = jpCSError "CMYK8 (Pixel CMYK Word8)" jimg


jpImageCMYK16ToImage :: Either String JP.DynamicImage -> Either String (Image VS CMYK Word16)
jpImageCMYK16ToImage (Right (JP.ImageCMYK16 jimg)) = Right (jpImageToImageUnsafe jimg)
jpImageCMYK16ToImage jimg = jpCSError "CMYK16 (Pixel CMYK Word16)" jimg


jpDynamicImageToImage' :: (Convertible JP.PixelCMYK16 (Pixel cs e),
                           Convertible JP.PixelCMYK8 (Pixel cs e),
                           Convertible JP.PixelRGB16 (Pixel cs e),
                           Convertible JP.PixelRGB8 (Pixel cs e),
                           Convertible JP.PixelRGBA16 (Pixel cs e),
                           Convertible JP.PixelRGBA8 (Pixel cs e),
                           Convertible JP.PixelRGBF (Pixel cs e),
                           Convertible JP.PixelYA16 (Pixel cs e),
                           Convertible JP.PixelYA8 (Pixel cs e),
                           Convertible JP.PixelYCbCr8 (Pixel cs e),
                           Convertible JP.Pixel16 (Pixel cs e),
                           Convertible JP.Pixel8 (Pixel cs e),
                           Convertible JP.PixelF (Pixel cs e),
                           Array arr cs e) =>
                          JP.DynamicImage -> Image arr cs e
jpDynamicImageToImage' (JP.ImageY8 jimg)     = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageY16 jimg)    = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageYF jimg)     = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageYA8 jimg)    = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageYA16 jimg)   = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageRGB8 jimg)   = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageRGB16 jimg)  = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageRGBF jimg)   = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageRGBA8 jimg)  = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageRGBA16 jimg) = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageYCbCr8 jimg) = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageCMYK8 jimg)  = jpImageToImageSafe jimg
jpDynamicImageToImage' (JP.ImageCMYK16 jimg) = jpImageToImageSafe jimg


jpDynamicImageToImage :: (Convertible JP.PixelCMYK16 (Pixel cs e),
                          Convertible JP.PixelCMYK8 (Pixel cs e),
                          Convertible JP.PixelRGB16 (Pixel cs e),
                          Convertible JP.PixelRGB8 (Pixel cs e),
                          Convertible JP.PixelRGBA16 (Pixel cs e),
                          Convertible JP.PixelRGBA8 (Pixel cs e),
                          Convertible JP.PixelRGBF (Pixel cs e),
                          Convertible JP.PixelYA16 (Pixel cs e),
                          Convertible JP.PixelYA8 (Pixel cs e),
                          Convertible JP.PixelYCbCr8 (Pixel cs e),
                          Convertible JP.Pixel16 (Pixel cs e),
                          Convertible JP.Pixel8 (Pixel cs e),
                          Convertible JP.PixelF (Pixel cs e), Array arr cs e) =>
                         Either String JP.DynamicImage -> Either String (Image arr cs e)
jpDynamicImageToImage = either jpError (Right . jpDynamicImageToImage')


jpImageShowCS :: JP.DynamicImage -> String
jpImageShowCS (JP.ImageY8 _)     = "Y8 (Pixel Y Word8)"
jpImageShowCS (JP.ImageY16 _)    = "Y16 (Pixel Y Word16)"
jpImageShowCS (JP.ImageYF _)     = "YF (Pixel Y Float)"
jpImageShowCS (JP.ImageYA8 _)    = "YA8 (Pixel YA Word8)"
jpImageShowCS (JP.ImageYA16 _)   = "YA16 (Pixel YA Word16)"
jpImageShowCS (JP.ImageRGB8 _)   = "RGB8 (Pixel RGB Word8)"
jpImageShowCS (JP.ImageRGB16 _)  = "RGB16 (Pixel RGB Word16)"
jpImageShowCS (JP.ImageRGBF _)   = "RGBF (Pixel RGB Float)"
jpImageShowCS (JP.ImageRGBA8 _)  = "RGBA8 (Pixel RGBA Word8)"
jpImageShowCS (JP.ImageRGBA16 _) = "RGBA16 (Pixel RGBA Word16)"
jpImageShowCS (JP.ImageYCbCr8 _) = "YCbCr8 (Pixel YCbCr Word8)"
jpImageShowCS (JP.ImageCMYK8 _)  = "CMYK8 (Pixel CMYK Word8)"
jpImageShowCS (JP.ImageCMYK16 _) = "CMYK16 (Pixel CMYK Word16)"


jpError :: String -> Either String a
jpError err = Left ("JuicyPixel decoding error: "++err)


jpCSError :: String -> Either String JP.DynamicImage -> Either String a
jpCSError _  (Left err)   = jpError err
jpCSError cs (Right jimg) =
  jpError $
  "Input image is in " ++
  jpImageShowCS jimg ++ ", cannot convert it to " ++ cs ++ " colorspace."


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


encodeGIFs :: (Array VS cs' e, Array VS cs Word8) =>
              [SaveOption [GIF]] -> (Pixel cs' e -> Pixel cs Word8)
           -> [(JP.GifDelay, Image VS cs' e)] -> BL.ByteString
encodeGIFs !opts !conv =
  either error id . JP.encodeGifImages (getGIFsLoop opts) . P.map palletizeGif where
    getGIFsLoop []                   = JP.LoopingNever
    getGIFsLoop (GIFsLooping loop:_) = loop
    getGIFsLoop (_:xs)               = getGIFsLoop xs    
    getGIFsPal []                      = JP.defaultPaletteOptions
    getGIFsPal (GIFsPalette palOpts:_) = palOpts
    getGIFsPal (_:xs)                  = getGIFsPal xs
    palletizeGif !(d, img) = (p, d, jimg) where  
      !(jimg, p) = JP.palettize (getGIFsPal opts) $
                   imageToJPImage (undefined :: JP.PixelRGB8) conv img


instance Writable [(JP.GifDelay, Image VS RGB Word8)] [GIF] where
  encode _ opts = encodeGIFs opts id

instance Writable [(JP.GifDelay, Image VS RGB Double)] [GIF] where
  encode _ opts = encodeGIFs opts (fmap toWord8)
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
{-# INLINE imageToJPImage #-}


    
