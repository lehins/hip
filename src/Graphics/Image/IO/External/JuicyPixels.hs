{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, ViewPatterns #-}
module Graphics.Image.IO.External.JuicyPixels (
  BMP(..),
  GIF(..), JP.GifDelay, JP.GifLooping(..), JP.PaletteOptions(..), JP.PaletteCreationMethod(..),
  HDR(..), JPG(..), PNG(..), TGA(..), TIF(..)
  ) where

import GHC.Float
import Data.Either
import Data.Monoid (mempty)
import Graphics.Image.ColorSpace
import Graphics.Image.Interface hiding (map)
import Graphics.Image.IO.Base
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Jpg as JP
import qualified Codec.Picture.Types as JP
import qualified Codec.Picture.ColorQuant as JP


data BMP = BMP -- ^ Bitmap image with @.bmp@ extension.

instance ImageFormat BMP where
  data SaveOption BMP

  ext _ = ".bmp"


data GIF = GIF -- ^ Graphics Interchange Format image with @.gif@ extension.

instance ImageFormat GIF where
  data SaveOption GIF = GIFPalette JP.PaletteOptions
  
  ext _ = ".gif"

instance ImageFormat [GIF] where
  data SaveOption [GIF] = GIFsPalette JP.PaletteOptions
                        | GIFsLooping JP.GifLooping

  ext _ = ext GIF

data HDR = HDR -- ^ High-dynamic-range image with @.hdr@ extension.

instance ImageFormat HDR where
  data SaveOption HDR

  ext _ = ".hdr"

  exts _ = [".hdr", ".pic"]


data JPG = JPG -- ^ Joint Photographic Experts Group image with @.jpg@ or
               -- @.jpeg@ extension.

instance ImageFormat JPG where
  data SaveOption JPG = JPGQuality Word8

  ext _ = ".jpg"

  exts _ = [".jpg", ".jpeg"]


data PNG = PNG -- ^ Portable Network Graphics image with @.png@ extension.

instance ImageFormat PNG where
  data SaveOption PNG

  ext _ = ".png"


data TGA = TGA -- ^ Truevision Graphics Adapter image with .tga extension.

instance ImageFormat TGA where
  data SaveOption TGA

  ext _ = ".tga"


data TIF = TIF -- ^ Tagged Image File Format image with @.tif@ or @.tiff@
               -- extension.

instance ImageFormat TIF where
  data SaveOption TIF  

  ext _ = ".tif"

  exts _ = [".tif", ".tiff"]


--------------------------------------------------------------------------------
-- Converting to and from JuicyPixels ------------------------------------------
--------------------------------------------------------------------------------

-- Y -> Y (Double)

instance Convertible JP.Pixel8 (Pixel Y Double) where
  convert = toDouble . PixelY

instance Convertible JP.Pixel16 (Pixel Y Double) where
  convert = toDouble . PixelY

instance Convertible JP.PixelF (Pixel Y Double) where
  convert = toDouble . PixelY

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
  convert (JP.PixelYA8 y a) = toDouble (PixelYA y a)

instance Convertible JP.PixelYA16 (Pixel YA Double) where
  convert (JP.PixelYA16 y a) = toDouble (PixelYA y a)


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
  convert = toPixelY . toDouble . (convert :: JP.PixelCMYK8 -> Pixel CMYK Word8)

instance Convertible JP.PixelCMYK16 (Pixel Y Double) where
  convert = toPixelY . toDouble . (convert :: JP.PixelCMYK16 -> Pixel CMYK Word16)

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
  convert = toDouble . fromChannel

instance Convertible JP.Pixel16 (Pixel RGB Double) where
  convert = toDouble . fromChannel

instance Convertible JP.PixelF (Pixel RGB Double) where
  convert = toDouble . fromChannel

instance Convertible JP.PixelYA8 (Pixel RGB Double) where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelYA16 (Pixel RGB Double) where
  convert = convert . JP.dropTransparency

-- Color -> RGB (Double)

instance Convertible JP.PixelRGB8 (Pixel RGB Double) where
  convert (JP.PixelRGB8 r g b) = toDouble $ PixelRGB r g b

instance Convertible JP.PixelRGB16 (Pixel RGB Double) where
  convert (JP.PixelRGB16 r g b) = toDouble $ PixelRGB r g b

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
  convert (JP.PixelRGBA8 r g b a) = toDouble $ PixelRGBA r g b a
  
instance Convertible JP.PixelRGBA16 (Pixel RGBA Double) where
  convert (JP.PixelRGBA16 r g b a) = toDouble $ PixelRGBA r g b a


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

instance (Array arr Y Word8, Array arr Binary Bit) => Readable (Image arr Binary Bit) BMP where
  decode _ = either Left (Right . toImageBinary) . jpImageY8ToImage . JP.decodeBitmap

instance Array arr Y Word8 => Readable (Image arr Y Word8) BMP where
  decode _ = jpImageY8ToImage . JP.decodeBitmap

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) BMP where
  decode _ = jpImageRGB8ToImage . JP.decodeBitmap

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) BMP where
  decode _ = jpImageRGBA8ToImage . JP.decodeBitmap

instance Array arr Y Double => Readable (Image arr Y Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Array arr YA Double => Readable (Image arr YA Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Array arr RGB Double => Readable (Image arr RGB Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap

instance Array arr RGBA Double => Readable (Image arr RGBA Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap


-- GIF Format Reading

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) GIF where
  decode _ = jpImageRGB8ToImage . JP.decodeGif

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) GIF where
  decode _ = jpImageRGBA8ToImage . JP.decodeGif

instance Array arr Y Double => Readable (Image arr Y Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Array arr YA Double => Readable (Image arr YA Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Array arr RGB Double => Readable (Image arr RGB Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

instance Array arr RGBA Double => Readable (Image arr RGBA Double) GIF where
  decode _ = jpDynamicImageToImage . JP.decodeGif

-- List of GIF Format frames Reading

decodeGifs :: (Either String JP.DynamicImage -> Either String img)
           -> B.ByteString -> Either String [img]
decodeGifs decoder = either Left decodeLS . JP.decodeGifImages where
    decodeLS ls = if null errs then Right imgs else Left $ unlines errs where
      (errs, imgs) = partitionEithers $ map (decoder . Right) ls

instance Array arr RGB Word8 => Readable [Image arr RGB Word8] [GIF] where
  decode _ = decodeGifs jpImageRGB8ToImage

instance Array arr RGBA Word8 => Readable [Image arr RGBA Word8] [GIF] where
  decode _ = decodeGifs jpImageRGBA8ToImage

instance Array arr Y Double => Readable [Image arr Y Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage

instance Array arr YA Double => Readable [Image arr YA Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage

instance Array arr RGB Double => Readable [Image arr RGB Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage

instance Array arr RGBA Double => Readable [Image arr RGBA Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage
  


-- HDR Format Reading

instance Array arr RGB Float => Readable (Image arr RGB Float) HDR where
  decode _ = jpImageRGBFToImage . JP.decodeHDR

instance Array arr Y Double => Readable (Image arr Y Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Array arr YA Double => Readable (Image arr YA Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Array arr RGB Double => Readable (Image arr RGB Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR

instance Array arr RGBA Double => Readable (Image arr RGBA Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR


-- JPG Format Reading

instance Array arr Y Word8 => Readable (Image arr Y Word8) JPG where
  decode _ = jpImageY8ToImage . JP.decodeJpeg

instance Array arr YA Word8 => Readable (Image arr YA Word8) JPG where
  decode _ = jpImageYA8ToImage . JP.decodeJpeg

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) JPG where
  decode _ = jpImageRGB8ToImage . JP.decodeJpeg

instance Array arr CMYK Word8 => Readable (Image arr CMYK Word8) JPG where
  decode _ = jpImageCMYK8ToImage . JP.decodeJpeg

instance Array arr YCbCr Word8 => Readable (Image arr YCbCr Word8) JPG where
  decode _ = jpImageYCbCr8ToImage . JP.decodeJpeg

instance Array arr Y Double => Readable (Image arr Y Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Array arr YA Double => Readable (Image arr YA Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Array arr RGB Double => Readable (Image arr RGB Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg

instance Array arr RGBA Double => Readable (Image arr RGBA Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg


-- PNG Format Reading

instance (Array arr Y Word8, Array arr Binary Bit) => Readable (Image arr Binary Bit) PNG where
  decode _ = either Left (Right . toImageBinary) . jpImageY8ToImage . JP.decodePng

instance Array arr Y Word8 => Readable (Image arr Y Word8) PNG where
  decode _ = jpImageY8ToImage . JP.decodePng

instance Array arr Y Word16 => Readable (Image arr Y Word16) PNG where
  decode _ = jpImageY16ToImage . JP.decodePng

instance Array arr YA Word8 => Readable (Image arr YA Word8) PNG where
  decode _ = jpImageYA8ToImage . JP.decodePng

instance Array arr YA Word16 => Readable (Image arr YA Word16) PNG where
  decode _ = jpImageYA16ToImage . JP.decodePng

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) PNG where
  decode _ = jpImageRGB8ToImage . JP.decodePng

instance Array arr RGB Word16 => Readable (Image arr RGB Word16) PNG where
  decode _ = jpImageRGB16ToImage . JP.decodePng

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) PNG where
  decode _ = jpImageRGBA8ToImage . JP.decodePng

instance Array arr RGBA Word16 => Readable (Image arr RGBA Word16) PNG where
  decode _ = jpImageRGBA16ToImage . JP.decodePng

instance Array arr Y Double => Readable (Image arr Y Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Array arr YA Double => Readable (Image arr YA Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Array arr RGB Double => Readable (Image arr RGB Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng

instance Array arr RGBA Double => Readable (Image arr RGBA Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng


-- TGA Format Reading

instance (Array arr Y Word8, Array arr Binary Bit) => Readable (Image arr Binary Bit) TGA where
  decode _ = either Left (Right . toImageBinary) . jpImageY8ToImage . JP.decodeTga

instance Array arr Y Word8 => Readable (Image arr Y Word8) TGA where
  decode _ = jpImageY8ToImage . JP.decodeTga

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) TGA where
  decode _ = jpImageRGB8ToImage . JP.decodeTga

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) TGA where
  decode _ = jpImageRGBA8ToImage . JP.decodeTga

instance Array arr Y Double => Readable (Image arr Y Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Array arr YA Double => Readable (Image arr YA Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Array arr RGB Double => Readable (Image arr RGB Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga

instance Array arr RGBA Double => Readable (Image arr RGBA Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga


-- TIF Format Reading

instance (Array arr Y Word8, Array arr Binary Bit) => Readable (Image arr Binary Bit) TIF where
  decode _ = either Left (Right . toImageBinary) . jpImageY8ToImage . JP.decodeTiff

instance Array arr Y Word8 => Readable (Image arr Y Word8) TIF where
  decode _ = jpImageY8ToImage . JP.decodeTiff

instance Array arr Y Word16 => Readable (Image arr Y Word16) TIF where
  decode _ = jpImageY16ToImage . JP.decodeTiff

instance Array arr YA Word8 => Readable (Image arr YA Word8) TIF where
  decode _ = jpImageYA8ToImage . JP.decodeTiff

instance Array arr YA Word16 => Readable (Image arr YA Word16) TIF where
  decode _ = jpImageYA16ToImage . JP.decodeTiff

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) TIF where
  decode _ = jpImageRGB8ToImage . JP.decodeTiff

instance Array arr RGB Word16 => Readable (Image arr RGB Word16) TIF where
  decode _ = jpImageRGB16ToImage . JP.decodeTiff

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) TIF where
  decode _ = jpImageRGBA8ToImage . JP.decodeTiff

instance Array arr RGBA Word16 => Readable (Image arr RGBA Word16) TIF where
  decode _ = jpImageRGBA16ToImage . JP.decodeTiff

instance Array arr CMYK Word8 => Readable (Image arr CMYK Word8) TIF where
  decode _ = jpImageCMYK8ToImage . JP.decodeTiff

instance Array arr CMYK Word16 => Readable (Image arr CMYK Word16) TIF where
  decode _ = jpImageCMYK16ToImage . JP.decodeTiff


instance Array arr Y Double => Readable (Image arr Y Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Array arr YA Double => Readable (Image arr YA Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Array arr RGB Double => Readable (Image arr RGB Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff

instance Array arr RGBA Double => Readable (Image arr RGBA Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff



-- General decoding and helper functions

jpImageToImage :: (Array arr cs e, Convertible jpx (Pixel cs e), JP.Pixel jpx) =>
                  JP.Image jpx -> Image arr cs e
jpImageToImage jimg = make (JP.imageHeight jimg, JP.imageWidth jimg) getPx
  where getPx (y, x) = convert $ JP.pixelAt jimg x y


jpImageY8ToImage :: Array arr Y Word8 =>
                    Either String JP.DynamicImage -> Either String (Image arr Y Word8)
jpImageY8ToImage (Right (JP.ImageY8 jimg)) = Right (jpImageToImage jimg)
jpImageY8ToImage jimg = jpCSError "Y8 (Pixel Y Word8)" jimg


jpImageY16ToImage :: Array arr Y Word16 =>
                     Either String JP.DynamicImage -> Either String (Image arr Y Word16)
jpImageY16ToImage (Right (JP.ImageY16 jimg)) = Right (jpImageToImage jimg)
jpImageY16ToImage jimg = jpCSError "Y16 (Pixel Y Word16)" jimg

{- -- No JuicyPixels images are actually read in this type
jpImageYFToImage :: Array arr Y Float =>
                     Either String JP.DynamicImage -> Either String (Image arr Y Float)
jpImageYFToImage (Right (JP.ImageYF jimg)) = Right (jpImageToImage jimg)
jpImageYFToImage jimg = jpCSError "YF (Pixel Y Float)" jimg
-}

jpImageYA8ToImage :: Array arr YA Word8 =>
                    Either String JP.DynamicImage -> Either String (Image arr YA Word8)
jpImageYA8ToImage (Right (JP.ImageYA8 jimg)) = Right (jpImageToImage jimg)
jpImageYA8ToImage jimg = jpCSError "YA8 (Pixel YA Word8)" jimg


jpImageYA16ToImage :: Array arr YA Word16 =>
                     Either String JP.DynamicImage -> Either String (Image arr YA Word16)
jpImageYA16ToImage (Right (JP.ImageYA16 jimg)) = Right (jpImageToImage jimg)
jpImageYA16ToImage jimg = jpCSError "YA16 (Pixel YA Word16)" jimg


jpImageRGB8ToImage :: Array arr RGB Word8 =>
                      Either String JP.DynamicImage -> Either String (Image arr RGB Word8)
jpImageRGB8ToImage (Right (JP.ImageRGB8 jimg)) = Right (jpImageToImage jimg)
jpImageRGB8ToImage jimg = jpCSError "RGB8 (Pixel RGB Word8)" jimg


jpImageRGB16ToImage :: Array arr RGB Word16 =>
                       Either String JP.DynamicImage -> Either String (Image arr RGB Word16)
jpImageRGB16ToImage (Right (JP.ImageRGB16 jimg)) = Right (jpImageToImage jimg)
jpImageRGB16ToImage jimg = jpCSError "RGB16 (Pixel RGB Word16)" jimg


jpImageRGBFToImage :: Array arr RGB Float =>
                       Either String JP.DynamicImage -> Either String (Image arr RGB Float)
jpImageRGBFToImage (Right (JP.ImageRGBF jimg)) = Right (jpImageToImage jimg)
jpImageRGBFToImage jimg = jpCSError "RGBF (Pixel RGB Float)" jimg


jpImageRGBA8ToImage :: Array arr RGBA Word8 =>
                      Either String JP.DynamicImage -> Either String (Image arr RGBA Word8)
jpImageRGBA8ToImage (Right (JP.ImageRGBA8 jimg)) = Right (jpImageToImage jimg)
jpImageRGBA8ToImage jimg = jpCSError "RGBA8 (Pixel RGBA Word8)" jimg


jpImageRGBA16ToImage :: Array arr RGBA Word16 =>
                       Either String JP.DynamicImage -> Either String (Image arr RGBA Word16)
jpImageRGBA16ToImage (Right (JP.ImageRGBA16 jimg)) = Right (jpImageToImage jimg)
jpImageRGBA16ToImage jimg = jpCSError "RGBA16 (Pixel RGBA Word16)" jimg


jpImageYCbCr8ToImage :: Array arr YCbCr Word8 =>
                      Either String JP.DynamicImage -> Either String (Image arr YCbCr Word8)
jpImageYCbCr8ToImage (Right (JP.ImageYCbCr8 jimg)) = Right (jpImageToImage jimg)
jpImageYCbCr8ToImage jimg = jpCSError "YCbCr8 (Pixel YCbCr Word8)" jimg


jpImageCMYK8ToImage :: Array arr CMYK Word8 =>
                      Either String JP.DynamicImage -> Either String (Image arr CMYK Word8)
jpImageCMYK8ToImage (Right (JP.ImageCMYK8 jimg)) = Right (jpImageToImage jimg)
jpImageCMYK8ToImage jimg = jpCSError "CMYK8 (Pixel CMYK Word8)" jimg


jpImageCMYK16ToImage :: Array arr CMYK Word16 =>
                      Either String JP.DynamicImage -> Either String (Image arr CMYK Word16)
jpImageCMYK16ToImage (Right (JP.ImageCMYK16 jimg)) = Right (jpImageToImage jimg)
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
jpDynamicImageToImage' (JP.ImageY8 jimg)     = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageY16 jimg)    = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageYF jimg)     = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageYA8 jimg)    = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageYA16 jimg)   = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageRGB8 jimg)   = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageRGB16 jimg)  = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageRGBF jimg)   = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageRGBA8 jimg)  = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageRGBA16 jimg) = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageYCbCr8 jimg) = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageCMYK8 jimg)  = jpImageToImage jimg
jpDynamicImageToImage' (JP.ImageCMYK16 jimg) = jpImageToImage jimg


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
jpCSError cs (Right jimg) = jpError ("Input image is in "++(jpImageShowCS jimg)++
                                     ", cannot convert it to "++cs++" colorspace.")


--------------------------------------------------------------------------------
-- Encoding images using JuicyPixels -------------------------------------------
--------------------------------------------------------------------------------

instance ManifestArray arr Y Word8 => Writable (Image arr Y Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel Y Word8 -> JP.Pixel8) 

instance ManifestArray arr RGB Word8 => Writable (Image arr RGB Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance ManifestArray arr RGBA Word8 => Writable (Image arr RGBA Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel RGBA Word8 -> JP.PixelRGBA8) 

instance ManifestArray arr Binary Bit => Writable (Image arr Binary Bit) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                                 . fromPixelBinary)

instance ManifestArray arr Y Double => Writable (Image arr Y Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                                 . toWord8)

instance ManifestArray arr YA Double => Writable (Image arr YA Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                                 . toWord8 . dropAlpha)

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                                 . toWord8)

instance ManifestArray arr RGBA Double => Writable (Image arr RGBA Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage ((convert :: Pixel RGBA Word8 -> JP.PixelRGBA8)
                                                 . toWord8)

-- Writable GIF

encodeGIF :: ManifestArray arr cs e =>
             [SaveOption GIF] -> (Pixel cs e -> JP.PixelRGB8)
             -> Image arr cs e -> BL.ByteString
encodeGIF []                     !conv =
  either error id . uncurry JP.encodeGifImageWithPalette .
  JP.palettize JP.defaultPaletteOptions . imageToJPImage conv
encodeGIF (GIFPalette palOpts:_) !conv =
  either error id . uncurry JP.encodeGifImageWithPalette .
  JP.palettize palOpts . imageToJPImage conv


instance ManifestArray arr RGB Word8 => Writable (Image arr RGB Word8) GIF where
  encode _ opts = encodeGIF opts (convert :: Pixel RGB Word8 -> JP.PixelRGB8)
  
instance ManifestArray arr Y Double => Writable (Image arr Y Double) GIF where
  encode _ opts = encodeGIF opts ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                  . toWord8 . toPixelRGB)
    
instance ManifestArray arr YA Double => Writable (Image arr YA Double) GIF where
  encode _ opts = encodeGIF opts ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                  . toWord8 . toPixelRGB . dropAlpha)

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) GIF where
  encode _ opts = encodeGIF opts ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                  . toWord8)

instance ManifestArray arr RGBA Double => Writable (Image arr RGBA Double) GIF where
  encode _ opts = encodeGIF opts ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                  . toWord8 . dropAlpha)


encodeGIFs :: ManifestArray arr cs e =>
              [SaveOption [GIF]] -> (Pixel cs e -> JP.PixelRGB8)
           -> [(JP.GifDelay, Image arr cs e)] -> BL.ByteString
encodeGIFs !opts !conv =
  either error id . JP.encodeGifImages (getGIFsLoop opts) . map palletizeGif where
    getGIFsLoop []                   = JP.LoopingNever
    getGIFsLoop (GIFsLooping loop:_) = loop
    getGIFsLoop (_:xs)               = getGIFsLoop xs    
    getGIFsPal []                      = JP.defaultPaletteOptions
    getGIFsPal (GIFsPalette palOpts:_) = palOpts
    getGIFsPal (_:xs)                  = getGIFsPal xs
    palletizeGif !(d, img) = (p, d, jimg) where  
      !(jimg, p) = JP.palettize (getGIFsPal opts) $ imageToJPImage conv img


instance ManifestArray arr RGB Word8 => Writable [(JP.GifDelay, Image arr RGB Word8)] [GIF] where
  encode _ opts = encodeGIFs opts (convert :: Pixel RGB Word8 -> JP.PixelRGB8)

instance ManifestArray arr RGB Double => Writable [(JP.GifDelay, Image arr RGB Double)] [GIF] where
  encode _ opts = encodeGIFs opts ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                   . toWord8)

-- Writable HDR

instance ManifestArray arr RGB Float => Writable (Image arr RGB Float) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage (convert :: Pixel RGB Float -> JP.PixelRGBF) 

instance ManifestArray arr Y Double => Writable (Image arr Y Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage ((convert :: Pixel RGB Float -> JP.PixelRGBF)
                                              . toFloat . toPixelRGB)

instance ManifestArray arr YA Double => Writable (Image arr YA Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage ((convert :: Pixel RGB Float -> JP.PixelRGBF)
                                              . toFloat . toPixelRGB . dropAlpha)

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage ((convert :: Pixel RGB Float -> JP.PixelRGBF)
                                              . toFloat)

instance ManifestArray arr RGBA Double => Writable (Image arr RGBA Double) HDR where
  encode _ _ = JP.encodeHDR . imageToJPImage ((convert :: Pixel RGB Float -> JP.PixelRGBF)
                                              . toFloat . dropAlpha)
 

-- Writable JPG


encodeJPG :: (JP.JpgEncodable px, ManifestArray arr cs e) =>
             [SaveOption JPG] -> (Pixel cs e -> px) -> Image arr cs e -> BL.ByteString
encodeJPG []               conv =
  JP.encodeDirectJpegAtQualityWithMetadata 100 mempty . imageToJPImage conv
encodeJPG (JPGQuality q:_) conv =
  JP.encodeDirectJpegAtQualityWithMetadata q mempty . imageToJPImage conv


instance ManifestArray arr Y Word8 => Writable (Image arr Y Word8) JPG where
  encode _ opts = encodeJPG opts (convert :: Pixel Y Word8 -> JP.Pixel8)

instance ManifestArray arr RGB Word8 => Writable (Image arr RGB Word8) JPG where
  encode _ opts = encodeJPG opts (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance ManifestArray arr CMYK Word8 => Writable (Image arr CMYK Word8) JPG where
  encode _ opts = encodeJPG opts (convert :: Pixel CMYK Word8 -> JP.PixelCMYK8) 
               
instance ManifestArray arr YCbCr Word8 => Writable (Image arr YCbCr Word8) JPG where
  encode _ opts = encodeJPG opts (convert :: Pixel YCbCr Word8 -> JP.PixelYCbCr8) 

instance ManifestArray arr Y Double => Writable (Image arr Y Double) JPG where
  encode _ opts = encodeJPG opts ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                  . toWord8) 

instance ManifestArray arr YA Double => Writable (Image arr YA Double) JPG where
  encode _ opts = encodeJPG opts ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                  . toWord8 . dropAlpha) 

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) JPG where
  encode _ opts = encodeJPG opts ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                  . toWord8) 

instance ManifestArray arr CMYK Double => Writable (Image arr CMYK Double) JPG where
  encode _ opts = encodeJPG opts ((convert :: Pixel CMYK Word8 -> JP.PixelCMYK8)
                                  . toWord8) 

instance ManifestArray arr YCbCr Double => Writable (Image arr YCbCr Double) JPG where
  encode _ opts = encodeJPG opts ((convert :: Pixel YCbCr Word8 -> JP.PixelYCbCr8)
                                  . toWord8) 

-- Writable PNG

instance ManifestArray arr Binary Bit => Writable (Image arr Binary Bit) PNG where
  encode _ _ = JP.encodePng . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8) 
                                              . fromPixelBinary)
  
instance ManifestArray arr Y Word8 => Writable (Image arr Y Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel Y Word8 -> JP.Pixel8) 

instance ManifestArray arr Y Word16 => Writable (Image arr Y Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel Y Word16 -> JP.Pixel16) 

instance ManifestArray arr YA Word8 => Writable (Image arr YA Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel YA Word8 -> JP.PixelYA8) 

instance ManifestArray arr YA Word16 => Writable (Image arr YA Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel YA Word16 -> JP.PixelYA16) 

instance ManifestArray arr RGB Word8 => Writable (Image arr RGB Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance ManifestArray arr RGB Word16 => Writable (Image arr RGB Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGB Word16 -> JP.PixelRGB16) 

instance ManifestArray arr RGBA Word8 => Writable (Image arr RGBA Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGBA Word8 -> JP.PixelRGBA8) 

instance ManifestArray arr RGBA Word16 => Writable (Image arr RGBA Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGBA Word16 -> JP.PixelRGBA16) 


instance ManifestArray arr Y Double => Writable (Image arr Y Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage ((convert :: Pixel Y Word16 -> JP.Pixel16)
                                              . toWord16)

instance ManifestArray arr YA Double => Writable (Image arr YA Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage ((convert :: Pixel YA Word16 -> JP.PixelYA16)
                                              . toWord16)

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage ((convert :: Pixel RGB Word16 -> JP.PixelRGB16)
                                              . toWord16)

instance ManifestArray arr RGBA Double => Writable (Image arr RGBA Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage ((convert :: Pixel RGBA Word16 -> JP.PixelRGBA16)
                                              . toWord16)

-- Writable TGA

instance ManifestArray arr Binary Bit => Writable (Image arr Binary Bit) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                              . fromPixelBinary)
  
instance ManifestArray arr Y Word8 => Writable (Image arr Y Word8) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (convert :: Pixel Y Word8 -> JP.Pixel8) 

instance ManifestArray arr RGB Word8 => Writable (Image arr RGB Word8) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance ManifestArray arr RGBA Word8 => Writable (Image arr RGBA Word8) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage (convert :: Pixel RGBA Word8 -> JP.PixelRGBA8) 


instance ManifestArray arr Y Double => Writable (Image arr Y Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                              . toWord8)

instance ManifestArray arr YA Double => Writable (Image arr YA Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                              . toWord8 . dropAlpha)

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage ((convert :: Pixel RGB Word8 -> JP.PixelRGB8)
                                              . toWord8)

instance ManifestArray arr RGBA Double => Writable (Image arr RGBA Double) TGA where
  encode _ _ = JP.encodeTga . imageToJPImage ((convert :: Pixel RGBA Word8 -> JP.PixelRGBA8)
                                              . toWord8)

-- Writable TIF

instance ManifestArray arr Y Word8 => Writable (Image arr Y Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel Y Word8 -> JP.Pixel8) 

instance ManifestArray arr Y Word16 => Writable (Image arr Y Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel Y Word16 -> JP.Pixel16) 

instance ManifestArray arr YA Word8 => Writable (Image arr YA Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel YA Word8 -> JP.PixelYA8) 

instance ManifestArray arr YA Word16 => Writable (Image arr YA Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel YA Word16 -> JP.PixelYA16) 

instance ManifestArray arr RGB Word8 => Writable (Image arr RGB Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance ManifestArray arr RGB Word16 => Writable (Image arr RGB Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel RGB Word16 -> JP.PixelRGB16) 

instance ManifestArray arr RGBA Word8 => Writable (Image arr RGBA Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel RGBA Word8 -> JP.PixelRGBA8) 

instance ManifestArray arr RGBA Word16 => Writable (Image arr RGBA Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel RGBA Word16 -> JP.PixelRGBA16) 

instance ManifestArray arr YCbCr Word8 => Writable (Image arr YCbCr Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel YCbCr Word8 -> JP.PixelYCbCr8)
  
instance ManifestArray arr CMYK Word8 => Writable (Image arr CMYK Word8) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel CMYK Word8 -> JP.PixelCMYK8) 

instance ManifestArray arr CMYK Word16 => Writable (Image arr CMYK Word16) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage (convert :: Pixel CMYK Word16 -> JP.PixelCMYK16) 


instance ManifestArray arr Binary Bit => Writable (Image arr Binary Bit) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel Y Word8 -> JP.Pixel8)
                                               . fromPixelBinary)
  
instance ManifestArray arr Y Double => Writable (Image arr Y Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel Y Word16 -> JP.Pixel16)
                                               . toWord16)

instance ManifestArray arr YA Double => Writable (Image arr YA Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel YA Word16 -> JP.PixelYA16)
                                               . toWord16)

instance ManifestArray arr RGB Double => Writable (Image arr RGB Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel RGB Word16 -> JP.PixelRGB16)
                                               . toWord16)

instance ManifestArray arr RGBA Double => Writable (Image arr RGBA Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel RGBA Word16 -> JP.PixelRGBA16)
                                               . toWord16)

instance ManifestArray arr YCbCr Double => Writable (Image arr YCbCr Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel YCbCr Word8 -> JP.PixelYCbCr8)
                                               . toWord8)

instance ManifestArray arr CMYK Double => Writable (Image arr CMYK Double) TIF where
  encode _ _ = JP.encodeTiff . imageToJPImage ((convert :: Pixel CMYK Word16 -> JP.PixelCMYK16)
                                               . toWord16)



imageToJPImage :: (JP.Pixel a, ManifestArray arr cs e) =>
                  (Pixel cs e -> a) -> Image arr cs e -> JP.Image a
imageToJPImage !f img@(dims -> (m, n)) = JP.generateImage g n m
  where g !j !i = f (index img (i, j))
        {-# INLINE g #-}
{-# INLINE imageToJPImage #-}


    
