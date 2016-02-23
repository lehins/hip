{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Graphics.Image.IO.External.NetPbm (
  PBM(..), PGM(..), PPM(..)
  ) where

import Graphics.Image.Interface
import Graphics.Image.ColorSpace
import Graphics.Image.IO.Base
import qualified Data.ByteString as B (ByteString)
import qualified Graphics.Netpbm as PNM
import qualified Data.Vector as V ((!))
import qualified Data.Vector.Storable as VS (convert, Vector)


data PBM = PBM -- ^ Netpbm: portable bitmap image with .pbm extension.

instance ImageFormat PBM where
  data SaveOption PBM

  ext _ = ".pbm"


data PGM = PGM -- ^ Netpbm: portable graymap image with .pgm extension.

instance ImageFormat PGM where
  data SaveOption PGM

  ext _ = ".pgm"


data PPM = PPM -- ^ Netpbm: portable pixmap image with .ppm extension.

instance ImageFormat PPM where
  data SaveOption PPM

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


-- BMP Format Reading
{-
instance Array arr Y Word8 => Readable (Image arr Y Word8) PGM where
  decode _ = pnmVectorY8ToImage . decodePnm

pnmVectorBToImage

pnmVectorY8ToImage (Right (PNM.PgmPixelData8 v)) = Right (pnmVectorToImage v)
jpImageYA8ToImage jimg = jpCSError "YA8 (Pixel YA Word8)" jimg

pnmVectorY16ToImage

pnmVectorRGB8ToImage

pnmVectorRGB16ToImage

pnmVectorToImage w h v = makeImage (h, w) getPx where
  getPx (i, j) = convert (v V.! (i * w + j))


--pnmImageToImage :: (Storable px1, AImage img px2, Pixel px2, Interconvertible px1 px2) =>
--                   PNM.PPM -> img px2
pnmImageToImage (PNM.PPM { PNM.ppmHeader = PNM.PPMHeader { PNM.ppmWidth  = width
                                                         , PNM.ppmHeight = height }
                         , PNM.ppmData   = ppmData }) = makeImage (height, width) getPx
  where getPx (i, j) = convert (vector V.! (i * width + j))
        vector = VS.convert $ getVector ppmData
        {-
        getVector (PNM.PpmPixelDataRGB8 v)  = v
        getVector (PNM.PpmPixelDataRGB16 v) = v
        getVector (PNM.PbmPixelData v)      = v
        getVector (PNM.PgmPixelData8 v)     = v
        getVector (PNM.PgmPixelData16 v)    = v
        -}

decodePpm :: B.ByteString -> Either String [PNM.PPM]
decodePpm = pnmResultToImage . PNM.parsePPM where
  pnmResultToImage (Right (ppms, _)) = Right ppms
  pnmResultToImage (Left err)        = Left ("Netpbm decoding error: "++err)
-}
