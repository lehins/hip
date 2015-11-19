{-# LANGUAGE BangPatterns, FlexibleContexts, MultiParamTypeClasses #-}
module Graphics.Image.Interface.Binary (
  Compareble (..), toBinary, toBinary2, fromBinary, invert
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface
import Graphics.Image.Interface.Pixel.Binary

class AImage img Binary => Compareble a b img where
  (.==.) :: a -> b -> img Binary  
  (./=.) :: a -> b -> img Binary  
  (.<.)  :: a -> b -> img Binary
  (.<=.) :: a -> b -> img Binary
  (.>.)  :: a -> b -> img Binary
  (.>=.) :: a -> b -> img Binary


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
