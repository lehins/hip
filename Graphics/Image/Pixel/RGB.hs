{-# LANGUAGE BangPatterns, FlexibleContexts, TemplateHaskell, TypeFamilies, ViewPatterns, MultiParamTypeClasses, UndecidableInstances #-}

module Graphics.Image.Pixel.RGB (
  RGB (..)
  ) where

import Graphics.Image.Interface (Pixel(..))
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)


data RGB = RGB !Double !Double !Double deriving Eq


instance Pixel RGB Double where
  pixel d = RGB d d d
  {-# INLINE pixel #-}

  pxOp f (RGB r g b) = RGB (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
  {-# INLINE pxOp2 #-}

  strongest (RGB r g b) = pixel . maximum $ [r, g, b]
  {-# INLINE strongest #-}

  weakest (RGB r g b) = pixel . minimum $ [r, g, b]
  {-# INLINE weakest #-}

  showType _ = "RGB"
  

instance Num RGB where
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
  
  fromInteger n = pixel . fromIntegral $ n
  {-# INLINE fromInteger #-}


instance Fractional RGB where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance Floating RGB where
  {-# INLINE pi #-}
  pi      = pixel pi
  {-# INLINE exp #-}
  exp     = pxOp exp
  {-# INLINE log #-}
  log     = pxOp log
  {-# INLINE sin #-}
  sin     = pxOp sin
  {-# INLINE cos #-}
  cos     = pxOp cos
  {-# INLINE asin #-}
  asin    = pxOp asin
  {-# INLINE atan #-}
  atan    = pxOp atan
  {-# INLINE acos #-}
  acos    = pxOp acos
  {-# INLINE sinh #-}
  sinh    = pxOp sinh
  {-# INLINE cosh #-}
  cosh    = pxOp cosh
  {-# INLINE asinh #-}
  asinh   = pxOp asinh
  {-# INLINE atanh #-}
  atanh   = pxOp atanh
  {-# INLINE acosh #-}
  acosh   = pxOp acosh


instance Ord RGB where
  (strongest -> RGB m1 _ _) <= (strongest -> RGB m2 _ _) = m1 <= m2
  {-# INLINE (<=) #-}

instance Show RGB where
  {-# INLINE show #-}
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


instance Elt RGB where
  {-# INLINE touch #-}
  touch (RGB r g b) = touch r >> touch g >> touch b
  
  {-# INLINE zero #-}
  zero             = 0

  {-# INLINE one #-}
  one              = 1


unboxRGB :: RGB -> (Double, Double, Double)
{-# INLINE unboxRGB #-}  
unboxRGB (RGB r g b) = (r, g, b)


boxRGB :: (Double, Double, Double) -> RGB
{-# INLINE boxRGB #-}
boxRGB (r, g, b) = RGB r g b


derivingUnbox "RGBPixel"
    [t| (Unbox Double) => RGB -> (Double, Double, Double) |]
    [| unboxRGB |]
    [| boxRGB |]
