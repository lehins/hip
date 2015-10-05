{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses,
UndecidableInstances, BangPatterns, FlexibleInstances, TypeFamilies #-}

module Graphics.Image.Color (
  Color (..), inRGB, inHSI
  ) where

import Prelude hiding (map)
import Graphics.Image.Definition (Pixel(..))
import Data.Int (Int8)
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)


data Color = RGB !Double !Double !Double
           | HSI !Double !Double !Double deriving Eq


inRGB :: Color -> Color
inRGB !(HSI !h !s !i) = RGB r g b where
    !r  = i + v1
    !g  = i - (v1/2) + v2
    !b  = i - (v1/2) - v2
    !v1 = c * s * (cos h)/3
    !v2 = c * s * (sin h)/2
    !c  = 2.44948974278318
inRGB px@(RGB _ _ _) = px


inHSI :: Color -> Color
inHSI (RGB r g b) = HSI h s i where
  h = if (v1 /= 0.0) then atan2 v2 v1 else 0
  s = sqrt((v1 * v1) + (v2 * v2))
  i = (r + g + b)/3
  v1 = (2.0*r - g - b) / c
  v2 = (g - b) / c
  c = 2.44948974278318
inHSI px@(HSI _ _ _) = px


instance Pixel Color where
  pixel d = RGB d d d
  {-# INLINE pixel #-}

  pxOp f (RGB r g b) = RGB (f r) (f g) (f b)
  pxOp f (HSI h s i) = HSI (f h) (f s) (f i)
  {-# INLINE pxOp #-}

  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
  pxOp2 f (HSI h1 s1 i1) (HSI h2 s2 i2) = HSI (f h1 h2) (f s1 s2) (f i1 i2)
  pxOp2 f px1 px2                       = pxOp2 f (inRGB px1) (inRGB px2)
  {-# INLINE pxOp2 #-}

  strongest (RGB r g b) = pixel . maximum $ [r, g, b]
  strongest px          = strongest . inRGB $ px
  {-# INLINE strongest #-}

  weakest (RGB r g b) = pixel . minimum $ [r, g, b]
  weakest px          = weakest . inRGB $ px
  {-# INLINE weakest #-}
  

instance Num Color where
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


instance Fractional Color where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance Floating Color where
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


instance Ord Color where
  (strongest -> RGB m1 _ _) <= (strongest -> RGB m2 _ _) = m1 <= m2
  px1 <= px2                                             = (inRGB px1) <= (inRGB px2)
  {-# INLINE (<=) #-}

instance Show Color where
  {-# INLINE show #-}
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"
  show (HSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"


instance Elt Color where
  {-# INLINE touch #-}
  touch (RGB r g b) = touch r >> touch g >> touch b
  touch (HSI h s i) = touch h >> touch s >> touch i
  
  {-# INLINE zero #-}
  zero             = 0

  {-# INLINE one #-}
  one              = 1


unboxColor :: Color -> (Int8, Double, Double, Double)
{-# INLINE unboxColor #-}  
unboxColor (RGB r g b) = (0, r, g, b)
unboxColor (HSI h s i) = (1, h, s, i)


boxColor :: (Int8, Double, Double, Double) -> Color
{-# INLINE boxColor #-}
boxColor (0, r, g, b) = RGB r g b
boxColor (1, h, s, i) = HSI h s i
boxColor u            = error ("Unsupported unboxed color: "++show u)


derivingUnbox "ColorPixel"
    [t| (Unbox Double) => Color -> (Int8, Double, Double, Double) |]
    [| unboxColor |]
    [| boxColor |]
