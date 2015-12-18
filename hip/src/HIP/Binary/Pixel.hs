{-# LANGUAGE BangPatterns, MultiParamTypeClasses, 
TypeFamilies, ViewPatterns, UndecidableInstances #-}

module HIP.Binary.Pixel (
  Binary(..), on, off, fromBool, isOn, isOff, inverted
  ) where

import Data.Bits
import HIP.Pixel.Base (Pixel(..))

-- Need to specify a newtype Bin in order to avoid installing Bool into Num

-- | This is a Binary pixel that can be created using these *constructors*:
--
--   [@'on'@] Represents value 'True' or @1@ in binary.
--
--   [@'off'@] Represents value 'False' or @0@ in binary.
--
newtype Binary = Binary Bool deriving Eq

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
isOn !(Binary v) = v
{-# INLINE isOn #-}


-- | Test if Pixel's value holds 'False'
isOff :: Binary -> Bool
isOff !(Binary v) = not v
{-# INLINE isOff #-}


inverted :: Binary -> Binary
inverted !(Binary v) = fromBool . not $ v
{-# INLINE inverted #-}

instance Pixel Binary where
  type Channel Binary = Bool
  
  fromDouble 0 = Binary False
  fromDouble _ = Binary True
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref !(Binary b) 0 = b
  ref !px         n = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  update _   0 !b = Binary b
  update !px n _  = error ("Updating "++show n++"is out of bounds for "++showType px)
  {-# INLINE update #-}
  
  apply !(f:_) !(Binary b) = Binary $ f b
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f:_) !(Binary b1) !(Binary b2) = Binary $ f b1 b2
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  showType _ = "Binary"


instance Num Binary where
  (+) !(Binary False) !(Binary False) = Binary False
  (+) _               _               = Binary True
  {-# INLINE (+) #-}

  (-) !(Binary True)  !(Binary True) = Binary False
  (-) !(Binary False) _              = Binary False
  (-) _               _              = Binary True
  {-# INLINE (-) #-}

  (*) !(Binary True) !(Binary True)  = Binary True
  (*) _              _               = Binary False
  {-# INLINE (*) #-}

  abs = id
  {-# INLINE abs #-}
  
  signum = id
  {-# INLINE signum #-}

  fromInteger 0 = Binary False
  fromInteger _ = Binary True
  {-# INLINE fromInteger #-}


instance Ord Binary where
  compare !(Binary v1) !(Binary v2) = compare v1 v2
  {-# INLINE compare #-}


instance Show Binary where
  show (Binary y) = "<Binary:("++b++")>" where b = if y then "1" else "0"


