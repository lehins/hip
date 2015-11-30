{-# LANGUAGE BangPatterns, FlexibleContexts, MultiParamTypeClasses #-}
module HIP.Binary (
  Compareble (..), toBinary, toBinary2, fromBinary, invert,
  erode, dialate, open, close
  ) where

import Prelude hiding (map, sum, zipWith)
import HIP.Interface
import HIP.Algorithms.Convolution
import HIP.Binary.Pixel
import HIP.Pixel.Base (Pixel(..))

-- | This is a very convenient set of functions that allow for binary image
-- construction. It is possible to compare either two images of same type
-- pointwise, or an image with an individual pixel, where this pixel will be
-- compared with each pixel in the image. For instance:
class AImage img Binary => Compareble a b img where
  (.==.) :: a -> b -> img Binary  
  (./=.) :: a -> b -> img Binary  
  (.<.)  :: a -> b -> img Binary
  (.<=.) :: a -> b -> img Binary
  (.>.)  :: a -> b -> img Binary
  (.>=.) :: a -> b -> img Binary

instance (Pixel px, Ord px, AImage img px, AImage img Binary)
         => Compareble (img px) (img px) img where
  (.==.) = toBinary2 (==)
  (./=.) = toBinary2 (/=)
  (.<.)  = toBinary2 (<)
  (.<=.) = toBinary2 (<=)
  (.>.)  = toBinary2 (>)
  (.>=.) = toBinary2 (>=)
  

instance (Pixel px, Ord px, AImage img px, AImage img Binary)
         => Compareble px (img px) img where
  (.==.) !px = toBinary (==px)
  (./=.) !px = toBinary (/=px)
  (.<.)  !px = toBinary (< px)
  (.<=.) !px = toBinary (<=px)
  (.>.)  !px = toBinary (> px)
  (.>=.) !px = toBinary (>=px)


instance (Pixel px, Ord px, AImage img px, AImage img Binary)
         => Compareble (img px) px img where
  (.==.) !img !px = toBinary (==px) img
  (./=.) !img !px = toBinary (/=px) img
  (.<.)  !img !px = toBinary (< px) img
  (.<=.) !img !px = toBinary (<=px) img
  (.>.)  !img !px = toBinary (> px) img
  (.>=.) !img !px = toBinary (>=px) img


toBinary :: (AImage img px, AImage img Binary) =>
            (px -> Bool)
         -> img px
         -> img Binary
toBinary !f !img = map (fromBool . f) img
{-# INLINE toBinary #-}


toBinary2 :: (AImage img px, AImage img Binary) =>
             (px -> px -> Bool)
          -> img px
          -> img px
          -> img Binary
toBinary2 !f =  zipWith (((.).(.)) fromBool f)
{-# INLINE toBinary2 #-}


fromBinary :: (AImage img Binary, AImage img px) =>
              img Binary
           -> img px
fromBinary !img = map toPx img where
  toPx !b = if isOn b then pixel 1 else pixel 0
  {-# INLINE toPx #-}
{-# INLINE fromBinary #-}
         

invert :: AImage img Binary => img Binary -> img Binary
invert = map inverted
{-# INLINE invert #-}


erode :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
         strat img Binary -> img Binary -> img Binary -> img Binary
erode strat !img' !img =
  (compute strat $ convolve Wrap img' img) .==. sum strat img'
{-# INLINE erode #-}


dialate :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
           strat img Binary -> img Binary -> img Binary -> img Binary
dialate strat !img' !img =
  (compute strat $ convolve Wrap img' img) ./=. off
{-# INLINE dialate #-}


open :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
        strat img Binary -> img Binary -> img Binary -> img Binary
open strat img = dialate strat img . erode strat img
{-# INLINE open #-}


close :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
        strat img Binary -> img Binary -> img Binary -> img Binary
close strat img = erode strat img . dialate strat img
{-# INLINE close #-}
