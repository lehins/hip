{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, MultiParamTypeClasses, 
             TypeFamilies, UndecidableInstances #-}

module HIP.Binary.Pixel (
  Binary(..), on, off, fromBool, isOn, isOff
  ) where

import Data.Bits
import Data.Data
import HIP.Pixel.Base (Pixel(..))

-- Need to specify a newtype Bin in order to avoid installing Bool into Num

-- | This is a Binary pixel that can be created using these __/constructors/__:
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
newtype Binary = Binary Bool deriving (Typeable, Data, Eq)

-- | Represents value 'True' or @1@ in binary. Often also called a foreground
-- pixel of an object.
on :: Binary
on = Binary True
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary. Often also called a background
-- pixel.
off :: Binary
off = Binary False
{-# INLINE off #-}


-- | Convert a 'Bool' to a 'Binary' pixel. @True == isOn $ fromBool True@
fromBool :: Bool -> Binary
fromBool = Binary
{-# INLINE fromBool #-}


-- | Test if Pixel's value holds 'True'
isOn :: Binary -> Bool
isOn (Binary v) = v
{-# INLINE isOn #-}


-- | Test if Pixel's value holds 'False'
isOff :: Binary -> Bool
isOff (Binary v) = not v
{-# INLINE isOff #-}


instance Pixel Binary where
  type Channel Binary = Bool
  
  fromDouble 0 = Binary False
  fromDouble _ = Binary True
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref (Binary b) 0 = b
  ref !px        n = error ("Referencing "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE ref #-}

  update _   0 !b = Binary b
  update !px n _  = error ("Updating "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE update #-}
  
  apply (f:_) (Binary b) = Binary $ f b
  apply _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply #-}

  apply2 (f:_) (Binary b1) (Binary b2) = Binary $ f b1 b2
  apply2 _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2 #-}

  maxChannel (Binary b) = b
  {-# INLINE maxChannel #-}

  minChannel (Binary b) = b
  {-# INLINE minChannel #-}

  fromChannel = Binary
  {-# INLINE fromChannel #-}

  

pxOp2 :: (Bool -> Bool -> Bool) -> Binary -> Binary -> Binary
pxOp2 f (Binary b1) (Binary b2) = Binary $ f b1 b2


instance Num Binary where
  (Binary False) + (Binary False) = Binary False
  _              + _              = Binary True
  {-# INLINE (+) #-}

  (Binary True)  - (Binary True) = Binary False
  (Binary False) - _             = Binary False
  _              - _             = Binary True
  {-# INLINE (-) #-}

  (Binary True) * (Binary True)  = Binary True
  _             * _              = Binary False
  {-# INLINE (*) #-}

  abs = id
  {-# INLINE abs #-}
  
  signum = id
  {-# INLINE signum #-}

  fromInteger 0 = Binary False
  fromInteger _ = Binary True
  {-# INLINE fromInteger #-}


instance Bits Binary where
  (.&.) = pxOp2 (.&.)
  {-# INLINE (.&.) #-}
  
  (.|.) = pxOp2 (.|.)
  {-# INLINE (.|.) #-}

  xor = pxOp2 xor
  {-# INLINE xor #-}

  complement (Binary b) = Binary $ complement b
  {-# INLINE complement #-}

  shift (Binary b) n = Binary $ shift b n
  {-# INLINE shift #-}

  rotate (Binary b) n = Binary $ rotate b n
  {-# INLINE rotate #-}

  bitSize (Binary b) = bitSize b 
  {-# INLINE bitSize #-}

  bitSizeMaybe (Binary b) = bitSizeMaybe b 
  {-# INLINE bitSizeMaybe #-}
  
  isSigned (Binary b) = isSigned b 
  {-# INLINE isSigned #-}

  testBit (Binary b) = testBit b 
  {-# INLINE testBit #-}

  bit = Binary . bit
  {-# INLINE bit #-}

  popCount (Binary b) = popCount b 
  {-# INLINE popCount #-}
  

instance Ord Binary where
  compare (Binary v1) (Binary v2) = compare v1 v2
  {-# INLINE compare #-}


instance Show Binary where
  show (Binary y) = "<Binary:("++b++")>" where b = if y then "1" else "0"


