{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.Image.External.JuicyPixels (
  PNG(..)
  ) where

import GHC.Float
import Data.Word (Word8, Word16)
import Graphics.Image.ColorSpace
import Graphics.Image.Interface
import Graphics.Image.External.Base
import qualified Data.ByteString as B (ByteString)
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP

data PNG = PNG


---- from JuicyPixels (Pixels) -----

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

instance Convertible PixelGray JP.PixelYA8 where
  convert = JP.promotePixel . (convert :: PixelGray -> JP.Pixel8)

instance Convertible PixelGray JP.PixelYA16 where
  convert = JP.promotePixel . (convert :: PixelGray -> JP.Pixel16)

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

instance Convertible PixelRGB JP.PixelYA8 where
  convert = convert . (convert :: PixelRGB -> PixelGray)

instance Convertible PixelRGB JP.PixelYA16 where
  convert = convert . (convert :: PixelRGB -> PixelGray)

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


instance ImageFormat PNG where
  ext _ = ".png"


instance Array arr cs e => Readable PNG (Image arr cs e) where

--  decode imgStr _ = decodeJPImageUsing JP.decodePng imgStr


jpImageToImage :: (Array arr cs e, Convertible jpx (Pixel cs e), JP.Pixel jpx) =>
                  JP.Image jpx -> Image arr cs e
jpImageToImage jimg = make (JP.imageHeight jimg, JP.imageWidth jimg) getPx
  where getPx (y, x) = convert $ JP.pixelAt jimg x y



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
                          Convertible JP.PixelF (Pixel cs e),
                          Array arr cs e) =>
                         JP.DynamicImage -> Image arr cs e
jpDynamicImageToImage (JP.ImageY8 jimg)     = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageY16 jimg)    = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageYF jimg)     = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageYA8 jimg)    = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageYA16 jimg)   = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageRGB8 jimg)   = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageRGB16 jimg)  = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageRGBF jimg)   = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageRGBA8 jimg)  = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageRGBA16 jimg) = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageYCbCr8 jimg) = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageCMYK8 jimg)  = jpImageToImage jimg
jpDynamicImageToImage (JP.ImageCMYK16 jimg) = jpImageToImage jimg

  

--decodeJPImageUsing :: (AImage img px, Pixel px, Interconvertible JP.DynamicImage (img px)) =>
--                      (B.ByteString -> Either String JP.DynamicImage)
--                      -> B.ByteString -> Either String (img px)
decodeJPImageUsing decoder imgstr = 
  either (Left . ("JuicyPixel decoding error: "++)) (Right . jpDynamicImageToImage) $ decoder imgstr
  
