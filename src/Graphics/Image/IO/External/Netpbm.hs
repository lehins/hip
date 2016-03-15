{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
-- |
-- Module      : Graphics.Image.IO.External.Netpbm
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.External.Netpbm (
  PBM(..), PGM(..), PPM(..)
  ) where


import Graphics.Image.ColorSpace
import Graphics.Image.Interface hiding (map)
import Graphics.Image.IO.Base
import Foreign.Storable (Storable)
import qualified Data.ByteString as B (ByteString)
import qualified Graphics.Netpbm as PNM
import qualified Data.Vector.Storable as VS ((!), Vector)


-- | Netpbm: portable bitmap image with @.pbm@ extension.
data PBM = PBM

instance ImageFormat PBM where
  data SaveOption PBM

  ext _ = ".pbm"


-- | Netpbm: portable graymap image with @.pgm@ extension.
data PGM = PGM

instance ImageFormat PGM where
  data SaveOption PGM

  ext _ = ".pgm"


-- | Netpbm: portable pixmap image with @.ppm@ extension.
data PPM = PPM

instance ImageFormat PPM where
  data SaveOption PPM

  ext _ = ".ppm"


instance ImageFormat [PBM] where
  data SaveOption [PBM]

  ext _ = ".pbm"


instance ImageFormat [PGM] where
  data SaveOption [PGM]

  ext _ = ".pgm"


instance ImageFormat [PPM] where
  data SaveOption [PPM]

  ext _ = ".ppm"


--------------------------------------------------------------------------------
-- Converting to and from Netpbm -----------------------------------------------
--------------------------------------------------------------------------------

-- -> Y (Double)

instance Convertible PNM.PbmPixel (Pixel Y Double) where
  convert (PNM.PbmPixel bool) = PixelY $ if bool then 0 else 1
  
instance Convertible PNM.PgmPixel8 (Pixel Y Double) where
  convert (PNM.PgmPixel8 w8) = toDouble . PixelY $ w8

instance Convertible PNM.PgmPixel16 (Pixel Y Double) where
  convert (PNM.PgmPixel16 w16) = toDouble . PixelY $ w16

instance Convertible PNM.PpmPixelRGB8 (Pixel Y Double) where
  convert (PNM.PpmPixelRGB8 r g b) = toPixelY . toDouble $ PixelRGB r g b

instance Convertible PNM.PpmPixelRGB16 (Pixel Y Double) where
  convert (PNM.PpmPixelRGB16 r g b) = toPixelY . toDouble $ PixelRGB r g b

-- -> YA (Double)

instance Convertible PNM.PbmPixel (Pixel YA Double) where
  convert = addAlpha 1 . (convert :: PNM.PbmPixel -> Pixel Y Double)
  
instance Convertible PNM.PgmPixel8 (Pixel YA Double) where
  convert = addAlpha 1 . (convert :: PNM.PgmPixel8 -> Pixel Y Double)

instance Convertible PNM.PgmPixel16 (Pixel YA Double) where
  convert = addAlpha 1 . (convert :: PNM.PgmPixel16 -> Pixel Y Double)

instance Convertible PNM.PpmPixelRGB8 (Pixel YA Double) where
  convert = addAlpha 1 . (convert :: PNM.PpmPixelRGB8 -> Pixel Y Double)

instance Convertible PNM.PpmPixelRGB16 (Pixel YA Double) where
  convert = addAlpha 1 . (convert :: PNM.PpmPixelRGB16 -> Pixel Y Double)

-- -> RGB (Double)

instance Convertible PNM.PbmPixel (Pixel RGB Double) where
  convert = toPixelRGB . (convert :: PNM.PbmPixel -> Pixel Y Double)
  
instance Convertible PNM.PgmPixel8 (Pixel RGB Double) where
  convert = toPixelRGB . (convert :: PNM.PgmPixel8 -> Pixel Y Double)

instance Convertible PNM.PgmPixel16 (Pixel RGB Double) where
  convert = toPixelRGB . (convert :: PNM.PgmPixel16 -> Pixel Y Double)

instance Convertible PNM.PpmPixelRGB8 (Pixel RGB Double) where
  convert (PNM.PpmPixelRGB8 r g b) = toDouble $ PixelRGB r g b

instance Convertible PNM.PpmPixelRGB16 (Pixel RGB Double) where
  convert (PNM.PpmPixelRGB16 r g b) = toDouble $ PixelRGB r g b


-- -> RGBA (Double)

instance Convertible PNM.PbmPixel (Pixel RGBA Double) where
  convert = addAlpha 1 . (convert :: PNM.PbmPixel -> Pixel RGB Double)
  
instance Convertible PNM.PgmPixel8 (Pixel RGBA Double) where
  convert = addAlpha 1 . (convert :: PNM.PgmPixel8 -> Pixel RGB Double)

instance Convertible PNM.PgmPixel16 (Pixel RGBA Double) where
  convert = addAlpha 1 . (convert :: PNM.PgmPixel16 -> Pixel RGB Double)

instance Convertible PNM.PpmPixelRGB8 (Pixel RGBA Double) where
  convert = addAlpha 1 . (convert :: PNM.PpmPixelRGB8 -> Pixel RGB Double)

instance Convertible PNM.PpmPixelRGB16 (Pixel RGBA Double) where
  convert = addAlpha 1 . (convert :: PNM.PpmPixelRGB16 -> Pixel RGB Double)


---- Exact precision conversions


instance Convertible PNM.PbmPixel (Pixel Binary Bit) where
  convert (PNM.PbmPixel bool) = fromBool bool
  
instance Convertible PNM.PgmPixel8 (Pixel Y Word8) where
  convert (PNM.PgmPixel8 w8) = PixelY w8

instance Convertible PNM.PgmPixel16 (Pixel Y Word16) where
  convert (PNM.PgmPixel16 w16) = PixelY w16

instance Convertible PNM.PpmPixelRGB8 (Pixel RGB Word8) where
  convert (PNM.PpmPixelRGB8 r g b) = PixelRGB r g b

instance Convertible PNM.PpmPixelRGB16 (Pixel RGB Word16) where
  convert (PNM.PpmPixelRGB16 r g b) = PixelRGB r g b


--------------------------------------------------------------------------------
-- Decoding images using Netpbm ------------------------------------------------
--------------------------------------------------------------------------------


-- BMP Format Reading (general)

instance Array arr Y Double => Readable (Image arr Y Double) PBM where
  decode _ = either Left (Right . ppmToImageUsing (pnmDataToImage id) . head) . decodePnm

instance Array arr Y Double => Readable (Image arr Y Double) PGM where
  decode _ = either Left (Right . ppmToImageUsing (pnmDataToImage id) . head) . decodePnm

instance Array arr Y Double => Readable (Image arr Y Double) PPM where
  decode _ = either Left (Right . ppmToImageUsing (pnmDataToImage id) . head) . decodePnm

instance Array arr YA Double => Readable (Image arr YA Double) PPM where
  decode _ = either Left (Right . ppmToImageUsing (pnmDataToImage (addAlpha 1)) . head) . decodePnm

instance Array arr RGB Double => Readable (Image arr RGB Double) PPM where
  decode _ = either Left (Right . ppmToImageUsing (pnmDataToImage id) . head) . decodePnm

instance Array arr RGBA Double => Readable (Image arr RGBA Double) PPM where
  decode _ = either Left (Right . ppmToImageUsing (pnmDataToImage (addAlpha 1)) . head) . decodePnm

-- BMP Format Reading (exact)

instance Array arr Binary Bit => Readable (Image arr Binary Bit) PBM where
  decode _ = either Left (ppmToImageUsing pnmDataPBMToImage . head) . decodePnm

instance Array arr Y Word8 => Readable (Image arr Y Word8) PGM where
  decode _ = either Left (ppmToImageUsing pnmDataPGM8ToImage . head) . decodePnm

instance Array arr Y Word16 => Readable (Image arr Y Word16) PGM where
  decode _ = either Left (ppmToImageUsing pnmDataPGM16ToImage . head) . decodePnm

instance Array arr RGB Word8 => Readable (Image arr RGB Word8) PPM where
  decode _ = either Left (ppmToImageUsing pnmDataPPM8ToImage . head) . decodePnm

instance Array arr RGB Word16 => Readable (Image arr RGB Word16) PPM where
  decode _ = either Left (ppmToImageUsing pnmDataPPM16ToImage . head) . decodePnm


instance Array arr Binary Bit => Readable [Image arr Binary Bit] [PBM] where
  decode _ = pnmToImagesUsing pnmDataPBMToImage

instance Array arr Y Word8 => Readable [Image arr Y Word8] [PGM] where
  decode _ = pnmToImagesUsing pnmDataPGM8ToImage

instance Array arr Y Word16 => Readable [Image arr Y Word16] [PGM] where
  decode _ = pnmToImagesUsing pnmDataPGM16ToImage

instance Array arr RGB Word8 => Readable [Image arr RGB Word8] [PPM] where
  decode _ = pnmToImagesUsing pnmDataPPM8ToImage

instance Array arr RGB Word16 => Readable [Image arr RGB Word16] [PPM] where
  decode _ = pnmToImagesUsing pnmDataPPM16ToImage


pnmToImagesUsing :: (Int -> Int -> PNM.PpmPixelData -> Either [Char] b)
                 -> B.ByteString -> Either String [b]
pnmToImagesUsing conv =
  either Left (Right . map (either error id . ppmToImageUsing conv)) . decodePnm


getPx :: (Storable a, Convertible a b) => VS.Vector a -> Int -> (Int, Int) -> b
getPx v w (i, j) = convert (v VS.! (i * w + j))


pnmDataToImage :: (Array arr cs e, Convertible PNM.PbmPixel px,
                   Convertible PNM.PgmPixel16 px, Convertible PNM.PgmPixel8 px,
                   Convertible PNM.PpmPixelRGB16 px, Convertible PNM.PpmPixelRGB8 px) =>
                  (px -> Pixel cs e) -> Int -> Int -> PNM.PpmPixelData -> Image arr cs e
pnmDataToImage conv w h (PNM.PbmPixelData v)      = makeImage (h, w) (conv . getPx v w)
pnmDataToImage conv w h (PNM.PgmPixelData8 v)     = makeImage (h, w) (conv . getPx v w)
pnmDataToImage conv w h (PNM.PgmPixelData16 v)    = makeImage (h, w) (conv . getPx v w)
pnmDataToImage conv w h (PNM.PpmPixelDataRGB8 v)  = makeImage (h, w) (conv . getPx v w)
pnmDataToImage conv w h (PNM.PpmPixelDataRGB16 v) = makeImage (h, w) (conv . getPx v w)


pnmDataPBMToImage :: (Array arr cs e, Convertible PNM.PbmPixel (Pixel cs e)) =>
                     Int -> Int -> PNM.PpmPixelData -> Either String (Image arr cs e)
pnmDataPBMToImage w h (PNM.PbmPixelData v) = Right $ makeImage (h, w) (getPx v w)
pnmDataPBMToImage _ _ d                    = pnmCSError "Binary (Pixel Binary Bit)" d

pnmDataPGM8ToImage :: (Array arr cs e, Convertible PNM.PgmPixel8 (Pixel cs e)) =>
                      Int -> Int -> PNM.PpmPixelData -> Either String (Image arr cs e)
pnmDataPGM8ToImage w h (PNM.PgmPixelData8 v) = Right $ makeImage (h, w) (getPx v w)
pnmDataPGM8ToImage _ _ d                     = pnmCSError "Y8 (Pixel Y Word8)" d

pnmDataPGM16ToImage :: (Array arr cs e, Convertible PNM.PgmPixel16 (Pixel cs e)) =>
                       Int -> Int -> PNM.PpmPixelData -> Either String (Image arr cs e)
pnmDataPGM16ToImage w h (PNM.PgmPixelData16 v) = Right $ makeImage (h, w) (getPx v w)
pnmDataPGM16ToImage _ _ d                      = pnmCSError "Y16 (Pixel Y Word16)" d

pnmDataPPM8ToImage :: (Array arr cs e, Convertible PNM.PpmPixelRGB8 (Pixel cs e)) =>
                      Int -> Int -> PNM.PpmPixelData -> Either String (Image arr cs e)
pnmDataPPM8ToImage w h (PNM.PpmPixelDataRGB8 v) = Right $ makeImage (h, w) (getPx v w)
pnmDataPPM8ToImage _ _ d                        = pnmCSError "RGB8 (Pixel RGB Word8)" d

pnmDataPPM16ToImage :: (Array arr cs e, Convertible PNM.PpmPixelRGB16 (Pixel cs e)) =>
                       Int -> Int -> PNM.PpmPixelData -> Either String (Image arr cs e)
pnmDataPPM16ToImage w h (PNM.PpmPixelDataRGB16 v) = Right $ makeImage (h, w) (getPx v w)
pnmDataPPM16ToImage _ _ d                         = pnmCSError "RGB16 (Pixel RGB Word16)" d


ppmToImageUsing :: (Int -> Int -> PNM.PpmPixelData -> t) -> PNM.PPM -> t
ppmToImageUsing conv (PNM.PPM { PNM.ppmHeader = PNM.PPMHeader { PNM.ppmWidth  = w
                                                              , PNM.ppmHeight = h }
                              , PNM.ppmData   = ppmData }) = conv w h ppmData
                                                        

decodePnm :: B.ByteString -> Either String [PNM.PPM]
decodePnm = pnmResultToImage . PNM.parsePPM where
  pnmResultToImage (Right ([], _))   = pnmError "Unknown"
  pnmResultToImage (Right (ppms, _)) = Right ppms
  pnmResultToImage (Left err)        = pnmError err




pnmError :: String -> Either String a
pnmError err = Left ("Netpbm decoding error: "++err)


pnmCSError :: String -> PNM.PpmPixelData -> Either String a
pnmCSError cs ppmData = pnmError ("Input image is in "++(pnmShowData ppmData)++
                                  ", cannot convert it to "++cs++" colorspace.")

pnmShowData :: PNM.PpmPixelData -> String
pnmShowData (PNM.PbmPixelData _)      = "Binary (Pixel Binary Bit)"
pnmShowData (PNM.PgmPixelData8 _)     = "Y8 (Pixel Y Word8)"
pnmShowData (PNM.PgmPixelData16 _)    = "Y16 (Pixel Y Word16)"
pnmShowData (PNM.PpmPixelDataRGB8 _)  = "RGB8 (Pixel RGB Word8)"
pnmShowData (PNM.PpmPixelDataRGB16 _) = "RGB8 (Pixel RGB Word8)"
