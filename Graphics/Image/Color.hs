{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns, FlexibleInstances #-}

module Graphics.Image.Color (
  RGB (..), HSI(..)
  ) where

import Graphics.Image.Gray
import Graphics.Image.Base as I
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

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

rad2pi r = if r < 0 then rad2pi (r + 2*pi) else
             if r >= (2*pi) then rad2pi (r - 2*pi) else r

to0 n = if isNaN n then 0 else n

instance Convertable RGB HSI where
  convert (RGB r g b) = HSI (to0 (h/(2*pi))) (to0 s) (to0 i) where
    h | b <= g = h'
      | otherwise = 2*pi - h' where
        h' = acos(0.5*(2*r - g - b)/(sqrt ((r-g)^2 + (r-b)*(g-b))))
    s = 1 - ((minimum [r, g, b])/i)
    i = (r+g+b)/3

instance Convertable HSI RGB where
  convert (HSI hdn s i) = RGB (to0 r) (to0 g) (to0 b) where
    h = rad2pi(hdn*2*pi)
    x = i*(1 - s)
    y h' = i*(1 + (s*cos h')/(cos (pi/3-h')))
    z y' = 3*i - x - y'
    (r,g,b) | h < 2*pi/3  = let y' = y h          in (y', z y', x)
            | h >= 4*pi/3 = let y' = y (h-4*pi/3) in (z y', x, y')
            | otherwise   = let y' = y (h-2*pi/3) in (x, y', z y')

instance Convertable (Image HSI) (Image RGB) where
  convert = I.map convert

instance Convertable (Image RGB) (Image HSI) where
  convert = I.map convert

crossImgs (a, b, c) = (f a, f b, f c) where
  f = V.map (\(Gray d) -> d) . I.toVector

instance Convertable (Image HSI) (Image Gray, Image Gray, Image Gray) where
  convert img = (I.map h img, I.map s img, I.map i img)
    where h (HSI v _ _) = Gray v
          s (HSI _ v _) = Gray v
          i (HSI _ _ v) = Gray v

instance Convertable (Image Gray, Image Gray, Image Gray) (Image HSI) where
  convert imgs@(himg, _, _) = fromVector m n $ V.zipWith3 HSI h s i
    where (h, s, i) = crossImgs imgs
          (m, n) = dims himg


{-
instance Convertable RGB HSI where
  convert (RGB r g b) = HSI h s i where
    h = (rad2pi (if (v1 /= 0.0) then atan2 v2 v1 else 0)) / (2*pi)
    s = 1 - ((minimum [r, g, b])/i)
    i = (r+g+b)/3
    v1 = (2.0*r-g-b) / c
    v2 = (g - b) / c
    c = 2.44948974278318

instance Convertable HSI RGB where
  convert (HSI h s i) = RGB r g b where
    h' = 2*pi
    r = i + v1
    g = i - (v1/2) + v2
    b = i - (v1/2) - v2
    v1 = c*s*(cos h')/3
    v2 = c*s*(sin h')/2
    c = 2.44948974278318
-}
