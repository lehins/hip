{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
module Graphics.Image.External (
  InputFormat(..),
  module Graphics.Image.External.Base,  
  module Graphics.Image.External.JuicyPixels
  ) where

import Graphics.Image.Interface
import Graphics.Image.External.Base
import Graphics.Image.External.JuicyPixels


-- | A collection of all image formats that can be read into HIP images with
-- 'Double` precision pixel channels.
data InputFormat = InputBMP
                 | InputGIF
                 | InputHDR
                 | InputJPG
                 | InputPNG
                 | InputTIF
                 | InputTGA  deriving (Show, Enum, Eq)


instance ImageFormat InputFormat where

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
