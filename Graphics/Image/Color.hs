{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns #-}

module Graphics.Image.Color (
  RGB (..)
  ) where

import Graphics.Image.Base
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V (Unbox)

data RGB = RGB !Double !Double !Double deriving Eq

instance Pixel RGB where
  {-# INLINE pixel #-}
  pixel d = RGB d d d

  {-# INLINE pxOp #-}
  pxOp f (RGB r g b) = RGB (f r) (f g) (f b)

  {-# INLINE pxOp2 #-}
  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)

  {-# INLINE strongest #-}
  strongest (RGB r g b) = pixel . maximum $ [r, g, b]

  {-# INLINE weakest #-}
  weakest (RGB r g b) = pixel . minimum $ [r, g, b]

instance Num RGB where
  {-# INLINE (+) #-}
  (+)           = pxOp2 (+)
  {-# INLINE (-) #-}
  (-)           = pxOp2 (-)
  {-# INLINE (*) #-}
  (*)           = pxOp2 (*)
  {-# INLINE abs #-}
  abs           = pxOp abs
  {-# INLINE signum #-}
  signum        = pxOp signum
  {-# INLINE fromInteger #-}
  fromInteger n = pixel . fromIntegral $ n

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
  {-# INLINE (<=) #-}
  (strongest -> RGB m1 _ _) <= (strongest -> RGB m2 _ _) = m1 <= m2

instance Show RGB where
  {-# INLINE show #-}
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


instance Elt RGB where
  {-# INLINE touch #-}
  touch (RGB r g b) = touch r >> touch g >> touch b
  
  {-# INLINE zero #-}
  zero = 0

  {-# INLINE one #-}
  one = 1

zipRGB :: RGB -> (Double, Double, Double)
{-# INLINE zipRGB #-}  
zipRGB (RGB r g b) = (r,g,b)

unzipRGB :: (Double, Double, Double) -> RGB
{-# INLINE unzipRGB #-}
unzipRGB (r,g,b) = (RGB r g b)


derivingUnbox "RGBPixel"
    [t| (V.Unbox Double) => RGB -> (Double, Double, Double) |]
    [| zipRGB |]
    [| unzipRGB |]


data HSI = HSI !Double !Double !Double deriving Eq

instance Pixel HSI where
  {-# INLINE pixel #-}
  pixel d = HSI d d d

  {-# INLINE pxOp #-}
  pxOp f (HSI r g b) = HSI (f r) (f g) (f b)

  {-# INLINE pxOp2 #-}
  pxOp2 f (HSI r1 g1 b1) (HSI r2 g2 b2) = HSI (f r1 r2) (f g1 g2) (f b1 b2)

  {-# INLINE strongest #-}
  strongest (HSI r g b) = pixel . maximum $ [r, g, b]

  {-# INLINE weakest #-}
  weakest (HSI r g b) = pixel . minimum $ [r, g, b]

instance Num HSI where
  {-# INLINE (+) #-}
  (+)           = pxOp2 (+)
  {-# INLINE (-) #-}
  (-)           = pxOp2 (-)
  {-# INLINE (*) #-}
  (*)           = pxOp2 (*)
  {-# INLINE abs #-}
  abs           = pxOp abs
  {-# INLINE signum #-}
  signum        = pxOp signum
  {-# INLINE fromInteger #-}
  fromInteger n = pixel . fromIntegral $ n

instance Fractional HSI where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n

instance Floating HSI where
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

instance Ord HSI where
  {-# INLINE (<=) #-}
  (strongest -> HSI m1 _ _) <= (strongest -> HSI m2 _ _) = m1 <= m2

instance Show HSI where
  {-# INLINE show #-}
  show (HSI r g b) = "<HSI:("++show r++"|"++show g++"|"++show b++")>"


instance Elt HSI where
  {-# INLINE touch #-}
  touch (HSI r g b) = touch r >> touch g >> touch b
  
  {-# INLINE zero #-}
  zero = 0

  {-# INLINE one #-}
  one = 1

zipHSI :: HSI -> (Double, Double, Double)
{-# INLINE zipHSI #-}  
zipHSI (HSI r g b) = (r,g,b)

unzipHSI :: (Double, Double, Double) -> HSI
{-# INLINE unzipHSI #-}
unzipHSI (r,g,b) = (HSI r g b)


derivingUnbox "HSIPixel"
    [t| (V.Unbox Double) => HSI -> (Double, Double, Double) |]
    [| zipHSI |]
    [| unzipHSI |]
