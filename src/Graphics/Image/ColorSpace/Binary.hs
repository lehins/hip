{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             TypeFamilies #-}
module Graphics.Image.ColorSpace.Binary (
  Binary(..), Bit(..), on, off, isOn, isOff, fromBool, complement
  ) where

import Prelude hiding (map)
import Data.Word (Word8)
import Graphics.Image.Interface
import Data.Typeable (Typeable)

-- | This is a Binary colorspace, pixel's of which can be created using
-- these __/constructors/__:
--
--   [@'on'@] Represents value @1@ or 'True'. It's a foreground pixel and is
--   displayed in black.
--
--   [@'off'@] Represents value @0@ or 'False'. It's a background pixel and is
--   displayed in white.
--
-- Note, that values are inverted before writing to or reading from file, since
-- grayscale images represent black as a @0@ value and white as @1@ on a
-- @[0,1]@ scale.
--
-- Binary pixels also behave as binary numbers with a size of 1-bit, for instance:
--
-- >>> on + on -- equivalent to: 1 .|. 1
-- <Binary:(1)>
-- >>> (on + on) * off -- equivalent to: (1 .|. 1) .&. 0
-- <Binary:(0)>
-- >>> (on + on) - on
-- <Binary:(0)>
--
data Binary = Binary deriving (Eq, Enum, Show, Typeable)


-- | Under the hood, Binary pixels are represented as 'Word8' that can only take
-- values of @0@ or @1@.
newtype Bit = Bit Word8 deriving (Ord, Eq, Typeable)


-- | Represents value 'True' or @1@ in binary. Often also called a foreground
-- pixel of an object.
on :: Pixel Binary Bit
on = PixelBinary (Bit 1)
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary. Often also called a background
-- pixel.
off :: Pixel Binary Bit
off = PixelBinary (Bit 0)
{-# INLINE off #-}


-- | Convert a 'Bool' to a 'PixelBin' pixel.
--
-- >>> isOn (fromBool True)
-- True
--
fromBool :: Bool -> Pixel Binary Bit
fromBool False = off
fromBool True  = on
{-# INLINE fromBool #-}


-- | Test if Pixel's value is 'on'.
isOn :: Pixel Binary Bit -> Bool
isOn (PixelBinary (Bit 0)) = False
isOn _                     = True
{-# INLINE isOn #-}


-- | Test if Pixel's value is 'off'.
isOff :: Pixel Binary Bit -> Bool
isOff = not . isOn
{-# INLINE isOff #-}


-- | Invert value of a pixel. Equivalent of 'not' for Bool's.
complement :: Pixel Binary Bit -> Pixel Binary Bit
complement = fromBool . isOff
{-# INLINE complement #-}



instance ColorSpace Binary where
  type PixelElt Binary e = e
  data Pixel Binary e = PixelBinary !e deriving Eq

  fromChannel = PixelBinary
  {-# INLINE fromChannel #-}

  fromElt = PixelBinary
  {-# INLINE fromElt #-}

  toElt (PixelBinary g) = g
  {-# INLINE toElt #-}

  getPxCh (PixelBinary g) _ = g
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelBinary g) = PixelBinary (f Binary g)
  {-# INLINE chOp #-}

  pxOp !f (PixelBinary g) = PixelBinary (f g)
  {-# INLINE pxOp #-}

  chApp (PixelBinary f) (PixelBinary b) = PixelBinary (f b)
  {-# INLINE chApp #-}


instance Show (Pixel Binary Bit) where
  show (PixelBinary (Bit 0)) = "<Binary:(0)>"
  show _                     = "<Binary:(1)>"


instance Num Bit where
  (Bit 0) + (Bit 0) = Bit 0
  _       + _       = Bit 1
  {-# INLINE (+) #-}
  
  _ - (Bit 1) = Bit 0
  _ - _       = Bit 1
  {-# INLINE (-) #-}
  
  _       * (Bit 0) = Bit 0
  (Bit 0) * _       = Bit 0
  _       * _       = Bit 1
  {-# INLINE (*) #-}
  
  abs         = id
  {-# INLINE abs #-}
  
  signum      = id
  {-# INLINE signum #-}
  
  fromInteger 0 = Bit 0
  fromInteger _ = Bit 1
  {-# INLINE fromInteger #-}
