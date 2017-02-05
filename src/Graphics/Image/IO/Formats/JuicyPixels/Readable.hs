{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.IO.Formats.JuicyPixels.Readable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats.JuicyPixels.Readable () where

import Prelude as P

import Graphics.Image.ColorSpace
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector
import Graphics.Image.IO.Base
import Graphics.Image.IO.Formats.JuicyPixels.Common
import qualified Data.ByteString as B  -- (ByteString)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Codec.Picture.Gif as JP
import qualified Data.Vector.Storable as V


--------------------------------------------------------------------------------
-- Converting to and from JuicyPixels ------------------------------------------
--------------------------------------------------------------------------------

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
  convert (PixelY y) = y
  
instance Convertible (Pixel Y Word16) JP.Pixel16 where
  convert (PixelY y) = y

instance Convertible (Pixel Y Word32) JP.Pixel32 where
  convert (PixelY y) = y

instance Convertible (Pixel Y Float) JP.PixelF where
  convert (PixelY y) = y

instance Convertible (Pixel YA Word8) JP.PixelYA8 where
  convert (PixelYA y a) = JP.PixelYA8 y a
  
instance Convertible (Pixel YA Word16) JP.PixelYA16 where
  convert (PixelYA y a) = JP.PixelYA16 y a

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


-- Animated GIF Format frames reading into a list

decodeGifs :: (Either String JP.DynamicImage -> Either String img)
           -> B.ByteString -> Either String [img]
decodeGifs decoder bs = do
  imgs <- JP.decodeGifImages bs
  sequence $ fmap (decoder . Right) imgs


decodeGifsDelays :: (Either String JP.DynamicImage -> Either String img)
                 -> B.ByteString -> Either String [(GifDelay, img)]
decodeGifsDelays decoder bs = do
  imgs <- JP.decodeGifImages bs
  delays <- JP.getDelaysGifImages bs
  gifs <- sequence $ fmap (decoder . Right) imgs
  return $ zip delays gifs



instance Readable [Image VS RGB Word8] GIFA where
  decode _ = decodeGifs jpImageRGB8ToImage

instance Readable [Image VS RGBA Word8] GIFA where
  decode _ = decodeGifs jpImageRGBA8ToImage

instance Readable [(GifDelay, Image VS RGB Word8)] GIFA where
  decode _ = decodeGifsDelays jpImageRGB8ToImage

instance Readable [(GifDelay, Image VS RGBA Word8)] GIFA where
  decode _ = decodeGifsDelays jpImageRGBA8ToImage


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

instance Readable (Image VS Y Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Readable (Image VS YA Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Readable (Image VS RGB Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Readable (Image VS RGBA Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap


instance Readable (Image VS Y Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Readable (Image VS YA Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Readable (Image VS RGB Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Readable (Image VS RGBA Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif


instance Readable [Image VS Y Double] GIFA where
  decode _ = decodeGifs jpDynamicImageToImage

instance Readable [Image VS YA Double] GIFA where
  decode _ = decodeGifs jpDynamicImageToImage

instance Readable [Image VS RGB Double] GIFA where
  decode _ = decodeGifs jpDynamicImageToImage

instance Readable [Image VS RGBA Double] GIFA where
  decode _ = decodeGifs jpDynamicImageToImage


instance Readable (Image VS Y Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Readable (Image VS YA Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Readable (Image VS RGB Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Readable (Image VS RGBA Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR


instance Readable (Image VS Y Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Readable (Image VS YA Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Readable (Image VS RGB Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Readable (Image VS RGBA Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg


instance Readable (Image VS Y Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Readable (Image VS YA Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Readable (Image VS RGB Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Readable (Image VS RGBA Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng


instance Readable (Image VS Y Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Readable (Image VS YA Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Readable (Image VS RGB Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Readable (Image VS RGBA Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga


instance Readable (Image VS Y Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Readable (Image VS YA Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Readable (Image VS RGB Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Readable (Image VS RGBA Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff



-- General decoding and helper functions

jpImageToImageUnsafe :: (Array VS cs e, JP.Pixel jpx) =>
                        JP.Image jpx -> Image VS cs e
jpImageToImageUnsafe (JP.Image n m !v) = fromVector (m, n) $ V.unsafeCast v



-- jpImageToImageSafe :: (Array arr cs e, Convertible jpx (Pixel cs e), JP.Pixel jpx) =>
--                       JP.Image jpx -> Image arr cs e
-- jpImageToImageSafe !jimg = makeImage (JP.imageHeight jimg, JP.imageWidth jimg) getPx
--   where getPx !(y, x) = convert $ JP.pixelAt jimg x y


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


jpDynamicImageToImage' :: (Convertible (Pixel Y Word8) (Pixel cs e),
                           Convertible (Pixel YA Word8) (Pixel cs e),
                           Convertible (Pixel Y Word16) (Pixel cs e),
                           Convertible (Pixel YA Word16) (Pixel cs e),
                           Convertible (Pixel Y Float) (Pixel cs e),
                           Convertible (Pixel RGB Word8) (Pixel cs e),
                           Convertible (Pixel RGBA Word8) (Pixel cs e),
                           Convertible (Pixel RGB Word16) (Pixel cs e),
                           Convertible (Pixel RGBA Word16) (Pixel cs e),
                           Convertible (Pixel RGB Float) (Pixel cs e),
                           Convertible (Pixel YCbCr Word8) (Pixel cs e),
                           Convertible (Pixel CMYK Word8) (Pixel cs e),
                           Convertible (Pixel CMYK Word16) (Pixel cs e),
                           Array VS cs e) =>
                          JP.DynamicImage -> Image VS cs e
jpDynamicImageToImage' (JP.ImageY8 jimg)     =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS Y Word8)
jpDynamicImageToImage' (JP.ImageYA8 jimg)    =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS YA Word8)
jpDynamicImageToImage' (JP.ImageY16 jimg)    =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS Y Word16)
jpDynamicImageToImage' (JP.ImageYA16 jimg)   =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS YA Word16)
jpDynamicImageToImage' (JP.ImageYF jimg)     =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS Y Float)
jpDynamicImageToImage' (JP.ImageRGB8 jimg)   =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS RGB Word8)
jpDynamicImageToImage' (JP.ImageRGBA8 jimg)  =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS RGBA Word8)
jpDynamicImageToImage' (JP.ImageRGB16 jimg)  =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS RGB Word16)
jpDynamicImageToImage' (JP.ImageRGBA16 jimg) =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS RGBA Word16)
jpDynamicImageToImage' (JP.ImageRGBF jimg)   =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS RGB Float)
jpDynamicImageToImage' (JP.ImageYCbCr8 jimg) =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS YCbCr Word8)
jpDynamicImageToImage' (JP.ImageCMYK8 jimg)  =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS CMYK Word8)
jpDynamicImageToImage' (JP.ImageCMYK16 jimg) =
  I.map convert $ (jpImageToImageUnsafe jimg :: Image VS CMYK Word16)


jpDynamicImageToImage :: (Convertible (Pixel Y Word8) (Pixel cs e),
                          Convertible (Pixel YA Word8) (Pixel cs e),
                          Convertible (Pixel Y Word16) (Pixel cs e),
                          Convertible (Pixel YA Word16) (Pixel cs e),
                          Convertible (Pixel Y Float) (Pixel cs e),
                          Convertible (Pixel RGB Word8) (Pixel cs e),
                          Convertible (Pixel RGBA Word8) (Pixel cs e),
                          Convertible (Pixel RGB Word16) (Pixel cs e),
                          Convertible (Pixel RGBA Word16) (Pixel cs e),
                          Convertible (Pixel RGB Float) (Pixel cs e),
                          Convertible (Pixel YCbCr Word8) (Pixel cs e),
                          Convertible (Pixel CMYK Word8) (Pixel cs e),
                          Convertible (Pixel CMYK Word16) (Pixel cs e),
                          Array VS cs e) =>
                         Either String JP.DynamicImage -> Either String (Image VS cs e)
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
