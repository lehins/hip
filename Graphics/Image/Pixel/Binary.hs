{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns #-}

module Graphics.Image.Binary (
  Binary (..)
  ) where

import Graphics.Image.Interface (Pixel(..))
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)

newtype Bin = Bin Bool deriving Eq

-- Need to specify a new type to avoid declaring instances for Bool
data Binary = Binary !Bin deriving Eq


instance Num Bin where
  (Bin False) + (Bin False)  = Bin False
  _     + _      = Bin True
  {-# INLINE (+) #-}

  (Bin True)  - (Bin True)   = Bin False
  (Bin False) - _      = Bin False
  _     - _      = Bin True
  {-# INLINE (-) #-}

  (Bin True)  * (Bin True)   = Bin True
  _     * _      = Bin False
  {-# INLINE (*) #-}

  abs v            = v
  {-# INLINE abs #-}
  signum v         = v
  {-# INLINE signum #-}

  fromInteger i = Bin $ if i == 0 then False else True
  {-# INLINE fromInteger #-}


instance Ord Bin where

  compare (Bin v1) (Bin v2) = compare v1 v2


instance Show Bin where
  show (Bin True)  = "1"
  show (Bin False) = "0"
  

instance Pixel Binary Bin where
  pixel 0                         = Binary 0
  pixel _                         = Binary 1
  {-# INLINE pixel #-}
  
  pxOp f (Binary b)               = Binary (f b)
  {-# INLINE pxOp #-}
  
  pxOp2 f (Binary b1) (Binary b2) = Binary (f b1 b2)
  {-# INLINE pxOp2 #-}

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


instance Elt Binary where
  touch (Binary y) = touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}

  one = 1
  {-# INLINE one #-}


instance Elt Bin where
  touch (Bin y) = touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}

  one = 1
  {-# INLINE one #-}


derivingUnbox "BinaryPixel"
    [t| (Unbox Bin) => Binary -> Bin |]
    [| \(Binary y) -> y |]
    [| \y -> Binary y |]

