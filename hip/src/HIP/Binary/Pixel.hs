{-# LANGUAGE BangPatterns, MultiParamTypeClasses, 
TypeFamilies, ViewPatterns, UndecidableInstances #-}

module HIP.Binary.Pixel (
  Binary(..), Bin(..), on, off, fromBool, isOn, isOff, inverted
  ) where

import HIP.Interface (Pixel(..))

-- Need to specify a new type to avoid declaring Num for Bool
newtype Bin = Bin Bool deriving Eq

-- | This is a Binary pixel that can be created using these *constructors*:
--
--   [@'on'@] Represents value 'True' or @1@ in binary.
--
--   [@'off'@] Represents value 'False' or @0@ in binary.
--
newtype Binary = Binary Bin deriving Eq

-- | Represents value 'True' or @1@ in binary.
on :: Binary
on = Binary . Bin $ True
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary.
off :: Binary
off = Binary . Bin $ False
{-# INLINE off #-}


-- | Convert a 'Bool' to a 'Binary' pixel. @True == isOn $ fromBool True@
fromBool :: Bool -> Binary
fromBool = Binary . Bin
{-# INLINE fromBool #-}


-- | Test if Pixel's value holds 'True'
isOn :: Binary -> Bool
isOn (Binary (Bin v)) = v
{-# INLINE isOn #-}


-- | Test if Pixel's value holds 'False'
isOff :: Binary -> Bool
isOff (Binary (Bin v)) = not v
{-# INLINE isOff #-}


inverted :: Binary -> Binary
inverted (Binary (Bin v)) = fromBool . not $ v
{-# INLINE inverted #-}


instance Num Bin where
  (Bin False) + (Bin False) = Bin False
  _           + _           = Bin True
  {-# INLINE (+) #-}

  (Bin True)  - (Bin True)  = Bin False
  (Bin False) - _           = Bin False
  _           - _           = Bin True
  {-# INLINE (-) #-}

  (Bin True)  * (Bin True)  = Bin True
  _           * _           = Bin False
  {-# INLINE (*) #-}

  abs !v            = v
  {-# INLINE abs #-}
  
  signum !v         = v
  {-# INLINE signum #-}

  fromInteger !i = Bin $ if i == 0 then False else True
  {-# INLINE fromInteger #-}


instance Ord Bin where

  compare (Bin v1) (Bin v2) = compare v1 v2


instance Show Bin where
  show (Bin True)  = "1"
  show (Bin False) = "0"
  

instance Pixel Binary where
  type Inner Binary = Bin
  pixel 0                         = off
  pixel _                         = on
  {-# INLINE pixel #-}
  
  pxOp !f !(Binary b)               = Binary (f b)
  {-# INLINE pxOp #-}
  
  pxOp2 !f !(Binary b1) !(Binary b2) = Binary (f b1 b2)
  {-# INLINE pxOp2 #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref 0 !(Binary b) = b
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}
  
  apply !(f:_) !(Binary b) = Binary $ f b
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f:_) !(Binary b1) !(Binary b2) = Binary $ f b1 b2
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  strongest                       = id
  {-# INLINE strongest #-}

  weakest                         = id
  {-# INLINE weakest #-}

  showType _                      = "Binary"


instance Num Binary where
  (+)           = pxOp2 (+)
  {-# INLINE (+) #-}

  (-)           = pxOp2 (-)
  {-# INLINE (-) #-}

  (*)           = pxOp2 (*)
  {-# INLINE (*) #-}

  abs           = pxOp abs
  {-# INLINE abs #-}

  signum        = pxOp signum
  {-# INLINE signum #-}

  fromInteger n = Binary . fromIntegral $ n
  {-# INLINE fromInteger #-}


instance Ord Binary where
  (Binary y1) <= (Binary y2) = y1 <= y2
  {-# INLINE (<=) #-}


instance Show Binary where
  show (Binary y) = "<Binary:("++show y++")>"
  {-# INLINE show #-}


