{-# LANGUAGE ConstraintKinds       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Graphics.Image.IO.Formats
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats
  ( module Graphics.Image.IO.Formats.JuicyPixels
#ifndef DISABLE_NETPBM
  , module Graphics.Image.IO.Formats.Netpbm
#endif
    -- * General
  , ImageFormat(..)
  , InputFormat(..)
  , OutputFormat(..)
  , Readable(..)
  , Writable(..)
  , Convertible(..)
  , Seq(..)
  , AllReadable
  , AllWritable
  , ComplexWritable
  ) where


import           Graphics.Image.Interface
import           Graphics.Image.IO.Base
import           Graphics.Image.IO.Formats.JuicyPixels
#ifndef DISABLE_NETPBM
import           Graphics.Image.IO.Formats.Netpbm
#endif




-- | A collection of all image formats that can be read into HIP images.
data InputFormat = InputBMP
                 | InputGIF
                 | InputHDR
                 | InputJPG
                 | InputPNG
                 | InputTIF
#ifndef DISABLE_NETPBM
                 | InputPNM
#endif
                 | InputTGA  deriving (Eq, Show, Enum, Bounded)


instance ImageFormat InputFormat where
  data SaveOption InputFormat

  ext InputBMP = ext BMP
  ext InputGIF = ext GIF
  ext InputHDR = ext HDR
  ext InputJPG = ext JPG
  ext InputPNG = ext PNG
  ext InputTGA = ext TGA
  ext InputTIF = ext TIF
#ifndef DISABLE_NETPBM
  ext InputPNM = ext PPM
#endif

  exts InputBMP = exts BMP
  exts InputGIF = exts GIF
  exts InputHDR = exts HDR
  exts InputJPG = exts JPG
  exts InputPNG = exts PNG
  exts InputTGA = exts TGA
  exts InputTIF = exts TIF
#ifndef DISABLE_NETPBM
  exts InputPNM = [ext PBM, ext PGM, ext PPM]
#endif


-- | Constraint type synonym for all readable formats.
type AllReadable arr cs = ( Readable (Image arr cs Double) BMP
                          , Readable (Image arr cs Double) GIF
                          , Readable (Image arr cs Double) HDR
                          , Readable (Image arr cs Double) JPG
                          , Readable (Image arr cs Double) PNG
                          , Readable (Image arr cs Double) TGA
                          , Readable (Image arr cs Double) TIF
#ifndef DISABLE_NETPBM
                          , Readable (Image arr cs Double) PPM
#endif
                          )


instance AllReadable arr cs => Readable (Image arr cs Double) InputFormat where
  decode InputBMP = decode BMP
  decode InputGIF = decode GIF
  decode InputHDR = decode HDR
  decode InputJPG = decode JPG
  decode InputPNG = decode PNG
  decode InputTIF = decode TIF
#ifndef DISABLE_NETPBM
  decode InputPNM = decode PPM
#endif
  decode InputTGA = decode TGA



-- | A collection of all image formats that can be written to file using images with
-- 'Double' precision pixels.
data OutputFormat = OutputBMP
                  | OutputGIF
                  | OutputHDR
                  | OutputJPG
                  | OutputPNG
                  | OutputTIF
                  | OutputTGA  deriving (Eq, Show, Enum, Bounded)


instance ImageFormat OutputFormat where
  data SaveOption OutputFormat
  ext OutputBMP = ext BMP
  ext OutputGIF = ext GIF
  ext OutputHDR = ext HDR
  ext OutputJPG = ext JPG
  ext OutputPNG = ext PNG
  ext OutputTGA = ext TGA
  ext OutputTIF = ext TIF

  exts OutputBMP = exts BMP
  exts OutputGIF = exts GIF
  exts OutputHDR = exts HDR
  exts OutputJPG = exts JPG
  exts OutputPNG = exts PNG
  exts OutputTGA = exts TGA
  exts OutputTIF = exts TIF


-- | Constraint type synonym for all writable formats.
type AllWritable arr cs = ( Writable (Image arr cs Double) BMP
                          , Writable (Image arr cs Double) GIF
                          , Writable (Image arr cs Double) HDR
                          , Writable (Image arr cs Double) JPG
                          , Writable (Image arr cs Double) PNG
                          , Writable (Image arr cs Double) TGA
                          , Writable (Image arr cs Double) TIF )


instance AllWritable arr cs => Writable (Image arr cs Double) OutputFormat where
  encode OutputBMP _ = encode BMP []
  encode OutputGIF _ = encode GIF []
  encode OutputHDR _ = encode HDR []
  encode OutputJPG _ = encode JPG []
  encode OutputPNG _ = encode PNG []
  encode OutputTGA _ = encode TGA []
  encode OutputTIF _ = encode TIF []
