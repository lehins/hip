{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ViewPatterns #-}
module Graphics.Image.External.JuicyPixels (
  BMP(..), GIF(..), HDR(..), JPG(..), PNG(..), TGA(..), TIF(..)
  ) where

import GHC.Float
import Data.Either
import Data.Word (Word8, Word16, Word32)
import Graphics.Image.ColorSpace
import Graphics.Image.Interface hiding (map)
import Graphics.Image.External.Base
import qualified Data.ByteString as B (ByteString)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP


data BMP = BMP -- ^ Bitmap image with @.bmp@ extension.

instance ImageFormat BMP where

  ext _ = ".bmp"


data GIF = GIF -- ^ Graphics Interchange Format image with @.gif@ extension.

instance ImageFormat GIF where

  ext _ = ".gif"

instance ImageFormat [GIF] where

  ext _ = ext GIF

data HDR = HDR -- ^ High-dynamic-range image with @.hdr@ extension.

instance ImageFormat HDR where

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

  ext _ = ".png"


data TGA = TGA -- ^ Truevision Graphics Adapter image with .tga extension.

instance ImageFormat TGA where

  ext _ = ".tga"


data TIF = TIF -- ^ Tagged Image File Format image with @.tif@ or @.tiff@
               -- extension.

instance ImageFormat TIF where

  ext _ = ".tif"

  exts _ = [".tif", ".tiff"]


--------------------------------------------------------------------------------
-- Converting to and from JuicyPixels ------------------------------------------
--------------------------------------------------------------------------------

-- Gray -> Gray

instance Convertible JP.Pixel8 PixelGray where
  convert = PixelGray . fromWord8

instance Convertible JP.Pixel16 PixelGray where
  convert = PixelGray . fromWord16

instance Convertible JP.PixelF PixelGray where
  convert = PixelGray . float2Double

instance Convertible JP.PixelYA8 PixelGray where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelYA16 PixelGray where
  convert = convert . JP.dropTransparency

-- Color -> Gray

instance Convertible JP.PixelRGB8 PixelGray where
  convert = convert . JP.computeLuma

instance Convertible JP.PixelRGB16 PixelGray where
  convert = convert . JP.computeLuma

instance Convertible JP.PixelRGBA8 PixelGray where
  convert = convert . JP.computeLuma

instance Convertible JP.PixelRGBA16 PixelGray where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelRGBF PixelGray where
  convert = convert . JP.computeLuma

instance Convertible JP.PixelYCbCr8 PixelGray where
  convert = convert . JP.computeLuma

instance Convertible JP.PixelCMYK8 PixelGray where
  convert = convert . (JP.convertPixel :: JP.PixelCMYK8 -> JP.PixelRGB8)

instance Convertible JP.PixelCMYK16 PixelGray where
  convert = convert . (JP.convertPixel :: JP.PixelCMYK16 -> JP.PixelRGB16)

-- Gray -> Color

instance Convertible JP.Pixel8 PixelRGB where
  convert = convert . (convert :: JP.Pixel8 -> PixelGray)

instance Convertible JP.Pixel16 PixelRGB where
  convert = convert . (convert :: JP.Pixel16 -> PixelGray)

instance Convertible JP.PixelF PixelRGB where
  convert = convert . (convert :: JP.PixelF -> PixelGray)

instance Convertible JP.PixelYA8 PixelRGB where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelYA16 PixelRGB where
  convert = convert . JP.dropTransparency

-- Color -> Color

instance Convertible JP.PixelRGB8 PixelRGB where
  convert (JP.PixelRGB8 r g b) = PixelRGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertible JP.PixelRGB16 PixelRGB where
  convert (JP.PixelRGB16 r g b) = PixelRGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertible JP.PixelRGBA8 PixelRGB where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelRGBA16 PixelRGB where
  convert = convert . JP.dropTransparency

instance Convertible JP.PixelRGBF PixelRGB where
  convert (JP.PixelRGBF r g b) = 
    PixelRGB (float2Double r) (float2Double g) (float2Double b)

instance Convertible JP.PixelYCbCr8 PixelRGB where
  convert = convert . (JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8)

instance Convertible JP.PixelCMYK8 PixelRGB where
  convert = convert . (JP.convertPixel :: JP.PixelCMYK8 -> JP.PixelRGB8)

instance Convertible JP.PixelCMYK16 PixelRGB where
  convert = convert . (JP.convertPixel :: JP.PixelCMYK16 -> JP.PixelRGB16)



---- to JuicyPixels -----


-- PixelGray -> PixelGray

instance Convertible PixelGray JP.Pixel8 where
  convert (PixelGray g) = toWord8 g

instance Convertible PixelGray JP.Pixel16 where
  convert (PixelGray g) = toWord16 g

instance Convertible PixelGray JP.PixelF where
  convert (PixelGray d) = double2Float d

instance Convertible PixelGrayA JP.PixelYA8 where
  convert (PixelGrayA g a) = JP.PixelYA8 (toWord8 g) (toWord8 a)

instance Convertible PixelGrayA JP.PixelYA16 where
  convert (PixelGrayA g a) = JP.PixelYA16 (toWord16 g) (toWord16 a)


-- PixelGray -> Color

instance Convertible PixelGray JP.PixelRGB8 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelRGB16 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelRGBA8 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelRGBA16 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelRGBF where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelYCbCr8 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelCMYK8 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

instance Convertible PixelGray JP.PixelCMYK16 where
  convert = convert . (convert :: PixelGray -> PixelRGB)

-- Color -> PixelGray

instance Convertible PixelRGB JP.Pixel8 where
  convert = convert . (convert :: PixelRGB -> PixelGray)

instance Convertible PixelRGB JP.Pixel16 where
  convert = convert . (convert :: PixelRGB -> PixelGray)
  
instance Convertible PixelRGB JP.PixelF where
  convert = convert . (convert :: PixelRGB -> PixelGray)

{-
instance Convertible PixelRGBA JP.PixelYA8 where
  convert = convert . (convert :: PixelRGBA -> PixelGray)

instance Convertible PixelRGBA JP.PixelYA16 where
  convert = convert . (convert :: PixelRGBA -> PixelGray)
-}

-- Color -> Color

instance Convertible PixelRGB JP.PixelRGB8 where
  convert (PixelRGB r g b) = JP.PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Convertible PixelRGB JP.PixelRGB16 where
  convert (PixelRGB r g b) = JP.PixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  
instance Convertible PixelRGB JP.PixelRGBA8 where
  convert = JP.promotePixel . (convert :: PixelRGB -> JP.PixelRGB8)

instance Convertible PixelRGB JP.PixelRGBA16 where
  convert = JP.promotePixel . (convert :: PixelRGB -> JP.PixelRGB16)

instance Convertible PixelRGB JP.PixelRGBF where
  convert (PixelRGB r g b) =
    JP.PixelRGBF (double2Float r) (double2Float g) (double2Float b)

instance Convertible PixelRGB JP.PixelYCbCr8 where
  convert = (JP.convertPixel :: JP.PixelRGB8 -> JP.PixelYCbCr8) . convert

instance Convertible PixelRGB JP.PixelCMYK8 where
  convert = (JP.convertPixel :: JP.PixelRGB8 -> JP.PixelCMYK8) . convert

instance Convertible PixelRGB JP.PixelCMYK16 where
  convert = (JP.convertPixel :: JP.PixelRGB16 -> JP.PixelCMYK16) . convert


instance Convertible PixelRGBA JP.PixelRGBA8 where
  convert (PixelRGBA r g b a) = JP.PixelRGBA8 (toWord8 r) (toWord8 g) (toWord8 b) (toWord8 a)

instance Convertible PixelRGBA JP.PixelRGBA16 where
  convert (PixelRGBA r g b a) = JP.PixelRGBA16 (toWord16 r) (toWord16 g) (toWord16 b) (toWord16 a)

  
---- Exact precision conversions

instance Convertible JP.Pixel8 (Pixel Gray Word8) where
  convert = PixelGray
  
instance Convertible JP.Pixel16 (Pixel Gray Word16) where
  convert = PixelGray

instance Convertible JP.Pixel32 (Pixel Gray Word32) where
  convert = PixelGray

instance Convertible JP.PixelF (Pixel Gray Float) where
  convert = PixelGray

instance Convertible JP.PixelYA8 (Pixel GrayA Word8) where
  convert (JP.PixelYA8 g a) = PixelGrayA g a
  
instance Convertible JP.PixelYA16 (Pixel GrayA Word16) where
  convert (JP.PixelYA16 g a) = PixelGrayA g a

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


instance Convertible (Pixel Gray Word8) JP.Pixel8 where
  convert (PixelGray g) = g
  
instance Convertible (Pixel Gray Word16) JP.Pixel16 where
  convert (PixelGray g) = g

instance Convertible (Pixel Gray Word32) JP.Pixel32 where
  convert (PixelGray g) = g

instance Convertible (Pixel Gray Float) JP.PixelF where
  convert (PixelGray g) = g

instance Convertible (Pixel GrayA Word8) JP.PixelYA8 where
  convert (PixelGrayA g a) = JP.PixelYA8 g a
  
instance Convertible (Pixel GrayA Word16) JP.PixelYA16 where
  convert (PixelGrayA g a) = JP.PixelYA16 g a

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



--------------------------------------------------------------------------------
-- Decoding images using JuicyPixels ------------------------------------------
--------------------------------------------------------------------------------


-- BMP Format Reading

instance Array arr Gray Word8 => Readable (Image arr Gray Word8) BMP where
  decode _ = jpImageY8ToImage . JP.decodeBitmap

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) BMP where
  decode _ = jpImageRGB8ToImage . JP.decodeBitmap

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) BMP where
  decode _ = jpImageRGBA8ToImage . JP.decodeBitmap

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) BMP where
  decode _ = jpDynamicImageToImage . JP.decodeBitmap


-- GIF Format Reading

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) GIF where
  decode _ = jpImageRGB8ToImage . JP.decodeGif

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) GIF where
  decode _ = jpImageRGBA8ToImage . JP.decodeGif

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) GIF where
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


instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable [Image arr cs Double] [GIF] where
  decode _ = decodeGifs jpDynamicImageToImage


-- HDR Format Reading

instance Array arr RGB Float => Readable (Image arr RGB Float) HDR where
  decode _ = jpImageRGBFToImage . JP.decodeHDR

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) HDR where
  decode _ = jpDynamicImageToImage . JP.decodeHDR



-- JPG Format Reading

instance Array arr Gray Word8 => Readable (Image arr Gray Word8) JPG where
  decode _ = jpImageY8ToImage . JP.decodeJpeg

instance Array arr GrayA Word8 => Readable (Image arr GrayA Word8) JPG where
  decode _ = jpImageYA8ToImage . JP.decodeJpeg

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) JPG where
  decode _ = jpImageRGB8ToImage . JP.decodeJpeg

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) JPG where
  decode _ = jpDynamicImageToImage . JP.decodeJpeg


-- PNG Format Reading

instance Array arr Gray Word8 => Readable (Image arr Gray Word8) PNG where
  decode _ = jpImageY8ToImage . JP.decodePng

instance Array arr Gray Word16 => Readable (Image arr Gray Word16) PNG where
  decode _ = jpImageY16ToImage . JP.decodePng

instance Array arr GrayA Word8 => Readable (Image arr GrayA Word8) PNG where
  decode _ = jpImageYA8ToImage . JP.decodePng

instance Array arr GrayA Word16 => Readable (Image arr GrayA Word16) PNG where
  decode _ = jpImageYA16ToImage . JP.decodePng

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) PNG where
  decode _ = jpImageRGB8ToImage . JP.decodePng

instance Array arr RGB Word16 => Readable (Image arr RGB Word16) PNG where
  decode _ = jpImageRGB16ToImage . JP.decodePng

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) PNG where
  decode _ = jpImageRGBA8ToImage . JP.decodePng

instance Array arr RGBA Word16 => Readable (Image arr RGBA Word16) PNG where
  decode _ = jpImageRGBA16ToImage . JP.decodePng

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) PNG where
  decode _ = jpDynamicImageToImage . JP.decodePng


-- TGA Format Reading

instance Array arr Gray Word8 => Readable (Image arr Gray Word8) TGA where
  decode _ = jpImageY8ToImage . JP.decodeTga

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) TGA where
  decode _ = jpImageRGB8ToImage . JP.decodeTga

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) TGA where
  decode _ = jpImageRGBA8ToImage . JP.decodeTga

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) TGA where
  decode _ = jpDynamicImageToImage . JP.decodeTga


-- TIF Format Reading

instance Array arr Gray Word8 => Readable (Image arr Gray Word8) TIF where
  decode _ = jpImageY8ToImage . JP.decodeTiff

instance Array arr Gray Word16 => Readable (Image arr Gray Word16) TIF where
  decode _ = jpImageY16ToImage . JP.decodeTiff

instance Array arr GrayA Word8 => Readable (Image arr GrayA Word8) TIF where
  decode _ = jpImageYA8ToImage . JP.decodeTiff

instance Array arr GrayA Word16 => Readable (Image arr GrayA Word16) TIF where
  decode _ = jpImageYA16ToImage . JP.decodeTiff

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) TIF where
  decode _ = jpImageRGB8ToImage . JP.decodeTiff

instance Array arr RGB Word16 => Readable (Image arr RGB Word16) TIF where
  decode _ = jpImageRGB16ToImage . JP.decodeTiff

instance Array arr RGBA Word8 => Readable (Image arr RGBA Word8) TIF where
  decode _ = jpImageRGBA8ToImage . JP.decodeTiff

instance Array arr RGBA Word16 => Readable (Image arr RGBA Word16) TIF where
  decode _ = jpImageRGBA16ToImage . JP.decodeTiff

instance (Convertible JP.PixelCMYK16 (Pixel cs Double),
          Convertible JP.PixelCMYK8 (Pixel cs Double),
          Convertible JP.PixelRGB16 (Pixel cs Double),
          Convertible JP.PixelRGB8 (Pixel cs Double),
          Convertible JP.PixelRGBA16 (Pixel cs Double),
          Convertible JP.PixelRGBA8 (Pixel cs Double),
          Convertible JP.PixelRGBF (Pixel cs Double),
          Convertible JP.PixelYA16 (Pixel cs Double),
          Convertible JP.PixelYA8 (Pixel cs Double),
          Convertible JP.PixelYCbCr8 (Pixel cs Double),
          Convertible JP.Pixel16 (Pixel cs Double),
          Convertible JP.Pixel8 (Pixel cs Double),
          Convertible JP.PixelF (Pixel cs Double), Array arr cs Double) =>
         Readable (Image arr cs Double) TIF where
  decode _ = jpDynamicImageToImage . JP.decodeTiff


-- General decoding and helper functions

jpImageToImage :: (Array arr cs e, Convertible jpx (Pixel cs e), JP.Pixel jpx) =>
                  JP.Image jpx -> Image arr cs e
jpImageToImage jimg = make (JP.imageHeight jimg, JP.imageWidth jimg) getPx
  where getPx (y, x) = convert $ JP.pixelAt jimg x y


jpImageY8ToImage :: Array arr Gray Word8 =>
                    Either String JP.DynamicImage -> Either String (Image arr Gray Word8)
jpImageY8ToImage (Right (JP.ImageY8 jimg)) = Right (jpImageToImage jimg)
jpImageY8ToImage jimg = jpCSError "Y8 (Pixel Gray Word8)" jimg


jpImageY16ToImage :: Array arr Gray Word16 =>
                     Either String JP.DynamicImage -> Either String (Image arr Gray Word16)
jpImageY16ToImage (Right (JP.ImageY16 jimg)) = Right (jpImageToImage jimg)
jpImageY16ToImage jimg = jpCSError "Y16 (Pixel Gray Word16)" jimg


jpImageYFToImage :: Array arr Gray Float =>
                     Either String JP.DynamicImage -> Either String (Image arr Gray Float)
jpImageYFToImage (Right (JP.ImageYF jimg)) = Right (jpImageToImage jimg)
jpImageYFToImage jimg = jpCSError "YF (Pixel Gray Float)" jimg


jpImageYA8ToImage :: Array arr GrayA Word8 =>
                    Either String JP.DynamicImage -> Either String (Image arr GrayA Word8)
jpImageYA8ToImage (Right (JP.ImageYA8 jimg)) = Right (jpImageToImage jimg)
jpImageYA8ToImage jimg = jpCSError "YA8 (Pixel GrayA Word8)" jimg


jpImageYA16ToImage :: Array arr GrayA Word16 =>
                     Either String JP.DynamicImage -> Either String (Image arr GrayA Word16)
jpImageYA16ToImage (Right (JP.ImageYA16 jimg)) = Right (jpImageToImage jimg)
jpImageYA16ToImage jimg = jpCSError "YA16 (Pixel GrayA Word16)" jimg


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
jpImageShowCS (JP.ImageY8 jimg)     = "Y8 (Pixel Gray Word8)"
jpImageShowCS (JP.ImageY16 jimg)    = "Y16 (Pixel Gray Word16)"
jpImageShowCS (JP.ImageYF jimg)     = "YF (Pixel Gray Float)"
jpImageShowCS (JP.ImageYA8 jimg)    = "YA8 (Pixel GrayA Word8)"
jpImageShowCS (JP.ImageYA16 jimg)   = "YA16 (Pixel GrayA Word16)"
jpImageShowCS (JP.ImageRGB8 jimg)   = "RGB8 (Pixel RGB Word8)"
jpImageShowCS (JP.ImageRGB16 jimg)  = "RGB16 (Pixel RGB Word16)"
jpImageShowCS (JP.ImageRGBF jimg)   = "RGBF (Pixel RGB Float)"
jpImageShowCS (JP.ImageRGBA8 jimg)  = "RGBA8 (Pixel RGBA Word8)"
jpImageShowCS (JP.ImageRGBA16 jimg) = "RGBA16 (Pixel RGBA Word16)"
jpImageShowCS (JP.ImageYCbCr8 jimg) = "YCbCr8"
jpImageShowCS (JP.ImageCMYK8 jimg)  = "CMYK8"
jpImageShowCS (JP.ImageCMYK16 jimg) = "CMYK16"


jpError :: String -> Either String a
jpError err = Left ("JuicyPixel decoding error: "++err)


jpCSError :: String -> Either String JP.DynamicImage -> Either String a
jpCSError _  (Left err)   = jpError err
jpCSError cs (Right jimg) = jpError ("Input image is in "++(jpImageShowCS jimg)++
                                     ", cannot convert it to "++cs++" colorspace.")


--------------------------------------------------------------------------------
-- Encoding images using JuicyPixels -------------------------------------------
--------------------------------------------------------------------------------


instance Array arr Gray Word8 => Writable (Image arr Gray Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel Gray Word8 -> JP.Pixel8) 

instance Array arr RGB Word8 => Writable (Image arr RGB Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance Array arr RGBA Word8 => Writable (Image arr RGBA Word8) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel RGBA Word8 -> JP.PixelRGBA8) 

instance Array arr Gray Double => Writable (Image arr Gray Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel Gray Double -> JP.Pixel8) 

instance Array arr RGB Double => Writable (Image arr RGB Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: Pixel RGB Double -> JP.PixelRGB8) 

instance Array arr RGBA Double => Writable (Image arr RGBA Double) BMP where
  encode _ _ = JP.encodeBitmap . imageToJPImage (convert :: PixelRGBA -> JP.PixelRGBA8) 



instance Array arr Gray Word8 => Writable (Image arr Gray Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel Gray Word8 -> JP.Pixel8) 

instance Array arr Gray Word16 => Writable (Image arr Gray Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel Gray Word16 -> JP.Pixel16) 

instance Array arr GrayA Word8 => Writable (Image arr GrayA Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel GrayA Word8 -> JP.PixelYA8) 

instance Array arr GrayA Word16 => Writable (Image arr GrayA Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel GrayA Word16 -> JP.PixelYA16) 

instance Array arr RGB Word8 => Writable (Image arr RGB Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGB Word8 -> JP.PixelRGB8) 

instance Array arr RGB Word16 => Writable (Image arr RGB Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGB Word16 -> JP.PixelRGB16) 

instance Array arr RGBA Word8 => Writable (Image arr RGBA Word8) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGBA Word8 -> JP.PixelRGBA8) 

instance Array arr RGBA Word16 => Writable (Image arr RGBA Word16) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGBA Word16 -> JP.PixelRGBA16) 


instance Array arr Gray Double => Writable (Image arr Gray Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel Gray Double -> JP.Pixel16) 

instance Array arr GrayA Double => Writable (Image arr GrayA Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel GrayA Double -> JP.PixelYA16) 

instance Array arr RGB Double => Writable (Image arr RGB Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: Pixel RGB Double -> JP.PixelRGB16) 

instance Array arr RGBA Double => Writable (Image arr RGBA Double) PNG where
  encode _ _ = JP.encodePng . imageToJPImage (convert :: PixelRGBA -> JP.PixelRGBA16)



imageToJPImage :: (JP.Pixel a, Array arr cs e) =>
                  (Pixel cs e -> a) -> Image arr cs e -> JP.Image a
imageToJPImage f img@(dims -> (m, n)) = JP.generateImage g n m
  where g j i = f $ index img (i, j)


    
