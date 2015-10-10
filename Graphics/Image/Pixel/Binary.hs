{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns #-}

module Graphics.Image.Binary (
  Binary (..)
  ) where

import Graphics.Image.Interface (Pixel(..))
import Data.Array.Repa.Eval
import Data.Int (Int8)
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Binary = Binary !Int8 deriving Eq


instance Pixel Binary where
  pixel b                         = Binary b
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


derivingUnbox "BinaryPixel"
    [t| (V.Unbox Double) => Binary -> Double |]
    [| \(Binary y) -> y |]
    [| \y -> Binary y |]

