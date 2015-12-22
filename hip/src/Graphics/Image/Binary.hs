{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.Image.Binary (
  -- * Thresholding
  B.Compareble (..),
  -- * Conversion
  toBinaryImageUsing, toBinaryImageUsing2,
  -- * Binary Operators
  (.&&.), (.||.), invert,
  -- * Binary Morphology
  -- $morphology
  erode, dialate, open, close,
  -- * Transformations.
  outline4, outline8, distanceTransform
  ) where

import Prelude hiding (zipWith)
import qualified HIP.Binary as B
import Graphics.Image.Pixel
import Graphics.Image.Internal


-- | Convert an image to a binary image by applying a function to each pixel.
--
-- >>> yield <- readImageRGB "images/yield.jpg"
-- >>> writeImage [] "images/yield_bin.png" $ toBinaryImageUsing (\(RGB r g b) -> r > g && r > b) yield
--
-- <<images/yield.jpg>> <<images/yield_bin.png>>
--
toBinaryImageUsing :: Pixel px =>
                      (px -> Bool) -- ^ Function that takes a pixel from a
                      -- source image and decides if new image will be 'on' or
                      -- 'off' at the same location in new binary image.
                   -> Image px     -- ^ Source image.
                   -> Image Binary
toBinaryImageUsing = B.toBinaryImageUsing
{-# INLINE toBinaryImageUsing #-}


toBinaryImageUsing2 :: Pixel px =>
             (px -> px -> Bool)
          -> Image px
          -> Image px
          -> Image Binary
toBinaryImageUsing2 = B.toBinaryImageUsing2
{-# INLINE toBinaryImageUsing2 #-}


-- | Pixel wise @AND@ operator on binary images. 
(.&&.) :: Image Binary -> Image Binary -> Image Binary
(.&&.) = (B..&&.)
{-# INLINE (.&&.) #-}


-- | Pixel wise @OR@ operator on binary images. 
(.||.) :: Image Binary -> Image Binary -> Image Binary
(.||.) = (B..||.)
{-# INLINE (.||.) #-}


-- | Pixel wise @NOT@ operator on binary images, i.e. creates a complement image.
invert :: Image Binary -> Image Binary
invert = B.invert
{-# INLINE invert #-}


{- $morphology In order to demonstrate how morphological operations work, a
/binary image/ = __B__ constructed here together with a /structuring element/ =
__S__ will be used in examples that follow.

@
figure :: Image Binary
figure = fromList [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,0],
                   [0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0],
                   [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                   [0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],
                   [0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],
                   [0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0],
                   [0,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0],
                   [0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
                   [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

struct :: Image Binary
struct = fromList [[0,1,0],[1,1,0],[0,1,0]]
@
-}


-- | Erosion is defined as: __{E = B ⊖ S = {m,n|Sₘₙ⊆B}__
--
-- >>> writeImage [Encoder inY8] "images/erode.png" $ pixelGrid 10 $ toGrayImage $ erode struct figure
--
-- <<images/figure.png>> eroded with <<images/struct.png>> is <<images/figure_erode.png>>
--
erode :: Image Binary -> Image Binary -> Image Binary
erode !img' = B.erode Identity img'
{-# INLINE erode #-}


-- | Dialation is defined as: __{D = B ⊕ S = {m,n|Sₘₙ∩B≠∅}__
--
-- >>> writeImage [Encoder inY8] "images/erode.png" $ pixelGrid 10 $ toGrayImage $ erode struct figure
--
-- <<images/figure.png>> dialated with <<images/struct.png>> is <<images/figure_dialate.png>>
--
dialate :: Image Binary -> Image Binary -> Image Binary
dialate !img' = B.dialate Identity img'
{-# INLINE dialate #-}


open :: Image Binary -> Image Binary -> Image Binary
open !img' = B.open Identity img'
{-# INLINE open #-}


close :: Image Binary -> Image Binary -> Image Binary
close !img' = B.close Identity img'
{-# INLINE close #-}


outline4 :: Image Binary -> Image Binary
outline4 = B.outline4
{-# INLINE outline4 #-}


outline8 :: Image Binary -> Image Binary
outline8 = B.outline8
{-# INLINE outline8 #-}


distanceTransform :: Image Binary -> Image Int
distanceTransform = B.distanceTransform Identity
{-# INLINE distanceTransform #-}


