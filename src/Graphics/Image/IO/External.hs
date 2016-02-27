{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, UndecidableInstances #-}
module Graphics.Image.IO.External (
  module Graphics.Image.IO.External.JuicyPixels,
  module Graphics.Image.IO.External.Netpbm,
  InputFormat, OutputFormat,
  Readable(..), Writable(..), ImageFormat(..),
  ) where

import Graphics.Image.Interface
import Graphics.Image.IO.Base
import Graphics.Image.IO.External.JuicyPixels
import Graphics.Image.IO.External.Netpbm


-- | A collection of all image formats that can be read into HIP images with
-- 'Double' precision pixel channels.
data InputFormat = InputBMP
                 | InputGIF
                 | InputHDR
                 | InputJPG
                 | InputPNG
                 | InputTIF
                 | InputPNM
                 | InputTGA  deriving (Show, Enum, Eq)


instance ImageFormat InputFormat where
  data SaveOption InputFormat

  ext InputBMP = ext BMP
  ext InputGIF = ext GIF
  ext InputHDR = ext HDR
  ext InputJPG = ext JPG
  ext InputPNG = ext PNG
  ext InputTGA = ext TGA
  ext InputTIF = ext TIF
  ext InputPNM = ext PPM

  exts InputBMP = exts BMP
  exts InputGIF = exts GIF
  exts InputHDR = exts HDR
  exts InputJPG = exts JPG
  exts InputPNG = exts PNG
  exts InputTGA = exts TGA
  exts InputTIF = exts TIF
  exts InputPNM = [ext PBM, ext PGM, ext PPM]


instance (Readable (Image arr cs Double) BMP,
          Readable (Image arr cs Double) GIF,
          Readable (Image arr cs Double) HDR,
          Readable (Image arr cs Double) JPG,
          Readable (Image arr cs Double) PNG,
          Readable (Image arr cs Double) TGA,
          Readable (Image arr cs Double) TIF,
          Readable (Image arr cs Double) PPM) =>
         Readable (Image arr cs Double) InputFormat where
  decode InputBMP = decode BMP
  decode InputGIF = decode GIF
  decode InputHDR = decode HDR
  decode InputJPG = decode JPG
  decode InputPNG = decode PNG
  decode InputTIF = decode TIF
  decode InputPNM = decode PPM
  decode InputTGA = decode TGA



-- | A collection of all image formats that can be written from HIP images with
-- 'Double' precision pixel channels.
data OutputFormat = OutputBMP
                  | OutputGIF
                  | OutputHDR
                  | OutputJPG
                  | OutputPNG
                  | OutputTIF
                  | OutputTGA  deriving (Show, Enum, Eq)


instance ImageFormat OutputFormat where
  data SaveOption OutputFormat
  ext OutputBMP = ext BMP
  ext OutputGIF = ext GIF
  ext OutputHDR = ext HDR
  ext OutputJPG = ext JPG
  ext OutputPNG = ext PNG
  ext OutputTGA = ext TGA
  ext OutputTIF = ext TIF


instance (Writable (Image arr cs Double) BMP,
          Writable (Image arr cs Double) GIF,
          Writable (Image arr cs Double) HDR,
          Writable (Image arr cs Double) JPG,
          Writable (Image arr cs Double) PNG,
          Writable (Image arr cs Double) TGA,
          Writable (Image arr cs Double) TIF) =>
         Writable (Image arr cs Double) OutputFormat where
  encode OutputBMP _ = encode BMP []
  encode OutputGIF _ = encode GIF []
  encode OutputHDR _ = encode HDR []
  encode OutputJPG _ = encode JPG []
  encode OutputPNG _ = encode PNG []
  encode OutputTGA _ = encode TGA []
  encode OutputTIF _ = encode TIF []