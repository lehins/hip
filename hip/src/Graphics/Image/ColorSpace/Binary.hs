{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.Binary (
  Binary(..), PixelBin, on, off, isOn, isOff, fromBool
  ) where

import Prelude hiding (map)
import Data.Word
import Graphics.Image.Interface

data Binary = Binary deriving (Eq, Enum)

-- | This is a Binaryary pixel that can be created using these __/constructors/__:
--
--   [@'on'@] Represents value 'True' or @1@ in binary. Represents objects as
--   black pixels when displayed.
--
--   [@'off'@] Represents value 'False' or @0@ in binary. Represents background
--   as white pixels when displayed.
--
-- Note, that values are inverted when written to or read from file, since
-- grayscale images represent black as a @0@ value and white as @1@ on a
-- @[0,1]@ scale.
--

newtype Bin = Bin Word8
type PixelBin = Pixel Binary Bin


-- | Represents value 'True' or @1@ in binary. Often also called a foreground
-- pixel of an object.
on :: PixelBin
on = PixelBin 1
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary. Often also called a background
-- pixel.
off :: PixelBin
off = PixelBin 0
{-# INLINE off #-}


-- | Convert a 'Bool' to a 'PixelBin' pixel. @True == isOn $ fromBool True@
fromBool :: Bool -> PixelBin
fromBool False = PixelBin 0
fromBool True  = PixelBin 1
{-# INLINE fromBool #-}


-- | Test if Pixel's value holds 'True'
isOn :: PixelBin -> Bool
isOn (PixelBin 0) = False
isOn (PixelBin _) = True
{-# INLINE isOn #-}


-- | Test if Pixel's value holds 'False'
isOff :: PixelBin -> Bool
isOff = not . isOn
{-# INLINE isOff #-}


instance ColorSpace Binary where
  type PixelElt Binary e = e
  data Pixel Binary e = PixelBin !e

  fromChannel = PixelBin
  {-# INLINE fromChannel #-}

  fromElt = PixelBin
  {-# INLINE fromElt #-}

  toElt (PixelBin g) = g
  {-# INLINE toElt #-}

  getPxCh (PixelBin g) _ = g
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelBin g) = PixelBin (f Binary g)
  {-# INLINE chOp #-}

  chOp2 !f (PixelBin g1) (PixelBin g2) = PixelBin (f Binary g1 g2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelBin g) = PixelBin (f g)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelBin g1) (PixelBin g2) = PixelBin (f g1 g2)
  {-# INLINE pxOp2 #-}
  

instance Show Binary where
  show _ = "Binary"


instance Show (Pixel Binary Bin) where
  show (PixelBin (Bin 0)) = "<Binary:(0)>"
  show (PixelBin _)       = "<Binary:(1)>"


instance Eq Bin where
  (Bin b1) == (Bin b2) = b1 == b2
  {-# INLINE (==) #-}
  

instance Num Bin where
  (Bin 0) + (Bin 0) = Bin 0
  _       + _       = Bin 1
  {-# INLINE (+) #-}
  
  _ - (Bin 1) = Bin 0
  _ - _       = Bin 1
  {-# INLINE (-) #-}
  
  _       * (Bin 0) = Bin 0
  (Bin 0) * _       = Bin 0
  _       * _       = Bin 1
  {-# INLINE (*) #-}
  
  abs         = id
  {-# INLINE abs #-}
  
  signum      = id
  {-# INLINE signum #-}
  
  fromInteger 0 = Bin 0
  fromInteger _ = Bin 1
  {-# INLINE fromInteger#-}
