{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.Image.Binary (
  B.Compareble (..), (.&&.), (.||.), toBinary, toBinary2, fromBinary, invert,
  erode, dialate, open, close, outline4, outline8, distanceTransform
  ) where

import Prelude hiding (zipWith)
import qualified HIP.Binary as B
import Graphics.Image.Pixel
import Graphics.Image.Internal



-- | Convert an image to a binary image by looking at each pixel with a function.
--
-- >>> yield <- readImageRGB "images/yield.jpg"
-- >>> writeImage [] "images/yield_bin.png" $ toBinary (\(RGB r g b) -> r < g || r < b) yield
--
-- <<images/yield.jpg>> <<images/yield_bin.png>>
--
toBinary :: Pixel px =>
            (px -> Bool) -- ^ Function that takes a pixel from a source image
                         -- and decides if new image will be 'on' or 'off' at
                         -- the same location in new binary image.
         -> Image px     -- ^ Source image.
         -> Image Binary
toBinary = B.toBinary
{-# INLINE toBinary #-}


toBinary2 :: Pixel px =>
             (px -> px -> Bool)
          -> Image px
          -> Image px
          -> Image Binary
toBinary2 = B.toBinary2
{-# INLINE toBinary2 #-}


fromBinary :: Pixel px => Image Binary -> Image px
fromBinary = B.fromBinary
{-# INLINE fromBinary #-}


(.&&.) :: Image Binary -> Image Binary -> Image Binary
(.&&.) = (B..&&.)
{-# INLINE (.&&.) #-}


(.||.) :: Image Binary -> Image Binary -> Image Binary
(.||.) = (B..||.)
{-# INLINE (.||.) #-}


-- | Flips all bits in the image.
invert :: Image Binary -> Image Binary
invert = B.invert
{-# INLINE invert #-}


erode :: Image Binary -> Image Binary -> Image Binary
erode !img' = B.erode Identity img'
{-# INLINE erode #-}


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


