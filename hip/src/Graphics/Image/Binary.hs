{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.Image.Binary (
  B.Compareble (..), toBinary, toBinary2, fromBinary, invert,
  erode
  ) where

import Prelude hiding (zipWith)
import qualified HIP.Binary as B
import Graphics.Image.Pixel
import Graphics.Image.Internal



-- | Convert an image to a binary image by looking at each pixel with a function.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> 
-- >>> let binFrog = toBinary (<0.9) frog
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


-- | Flips all bits in the image.
invert :: Image Binary -> Image Binary
invert = B.invert



erode :: Image Binary -> Image Binary -> Image Binary
erode !img' = B.erode Identity img'


