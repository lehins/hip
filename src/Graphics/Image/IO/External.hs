{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, UndecidableInstances #-}
module Graphics.Image.IO.External (
  module Graphics.Image.IO.External.JuicyPixels,
  InputFormat, OutputFormat,
  Readable(..), Writable(..), ImageFormat(..),
  ) where

import Graphics.Image.Interface
import Graphics.Image.IO.Base
import Graphics.Image.IO.External.JuicyPixels


-- | A collection of all image formats that can be read into HIP images with
-- 'Double' precision pixel channels.
data InputFormat = InputBMP
                 | InputGIF
                 | InputHDR
                 | InputJPG
                 | InputPNG
                 | InputTIF
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


instance (Readable (Image arr cs Double) BMP,
          Readable (Image arr cs Double) GIF,
          Readable (Image arr cs Double) HDR,
          Readable (Image arr cs Double) JPG,
          Readable (Image arr cs Double) PNG,
          Readable (Image arr cs Double) TGA,
          Readable (Image arr cs Double) TIF) =>
         Readable (Image arr cs Double) InputFormat where
  decode InputBMP = decode BMP
  decode InputGIF = decode GIF
  decode InputHDR = decode HDR
  decode InputJPG = decode JPG
  decode InputPNG = decode PNG
  decode InputTGA = decode TGA
  decode InputTIF = decode TIF



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
