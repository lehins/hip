{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Image.Unboxed.Binary (
  B.Compareble (..), toBinary, toBinary2, fromBinary, invert
  ) where

import Prelude hiding (zipWith)
import qualified Graphics.Image.Interface.Binary as B
import Graphics.Image.Interface.Pixel (pixel)
import Graphics.Image.Unboxed.Pixel
import Graphics.Image.Unboxed.Internal



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


instance (Pixel px, Ord px) => B.Compareble (Image px) (Image px) Image where
  (.==.) = toBinary2 (==)
  (./=.) = toBinary2 (/=)
  (.<.)  = toBinary2 (<)
  (.<=.) = toBinary2 (<=)
  (.>.)  = toBinary2 (>)
  (.>=.) = toBinary2 (>=)

instance (Pixel px, Ord px) => B.Compareble px (Image px) Image where
  (.==.) !px = toBinary (==px)
  (./=.) !px = toBinary (/=px)
  (.<.)  !px = toBinary (< px)
  (.<=.) !px = toBinary (<=px)
  (.>.)  !px = toBinary (> px)
  (.>=.) !px = toBinary (>=px)


instance (Pixel px, Ord px) => B.Compareble (Image px) px Image where
  (.==.) !img !px = toBinary (==px) img
  (./=.) !img !px = toBinary (/=px) img
  (.<.)  !img !px = toBinary (< px) img
  (.<=.) !img !px = toBinary (<=px) img
  (.>.)  !img !px = toBinary (> px) img
  (.>=.) !img !px = toBinary (>=px) img

instance (Pixel px, Ord px, Inner px ~ Double) => B.Compareble Double (Image px) Image where
  (.==.) !a = toBinary (==pixel a)
  (./=.) !a = toBinary (/=pixel a)
  (.<.)  !a = toBinary (< pixel a)
  (.<=.) !a = toBinary (<=pixel a)
  (.>.)  !a = toBinary (> pixel a)
  (.>=.) !a = toBinary (>=pixel a)


instance (Pixel px, Ord px, Inner px ~ Double) => B.Compareble (Image px) Double Image where
  (.==.) !img !a = toBinary (==pixel a) img
  (./=.) !img !a = toBinary (/=pixel a) img
  (.<.)  !img !a = toBinary (< pixel a) img
  (.<=.) !img !a = toBinary (<=pixel a) img
  (.>.)  !img !a = toBinary (> pixel a) img
  (.>=.) !img !a = toBinary (>=pixel a) img
