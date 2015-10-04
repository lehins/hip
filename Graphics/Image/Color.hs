{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns, FlexibleInstances #-}

module Graphics.Image.Color (
  RGB (..), HSI(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Definition (Pixel(..))
import Graphics.Image.Internal hiding (maximum, minimum)
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)
import qualified Codec.Picture as JP


data RGB = RGB !Double !Double !Double deriving Eq


data HSI = HSI !Double !Double !Double deriving Eq


instance Pixel RGB where
  pixel d                               = RGB d d d
  {-# INLINE pixel #-}

  pxOp f (RGB r g b)                    = RGB (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
  {-# INLINE pxOp2 #-}

  strongest (RGB r g b)                 = pixel . maximum $ [r, g, b]
  {-# INLINE strongest #-}

  weakest (RGB r g b)                   = pixel . minimum $ [r, g, b]
  {-# INLINE weakest #-}
  

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
  {-# INLINE (<=) #-}
  (strongest -> RGB m1 _ _) <= (strongest -> RGB m2 _ _) = m1 <= m2


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


instance Pixel HSI where
  pixel d                               = HSI d d d
  {-# INLINE pixel #-}

  pxOp f (HSI r g b)                    = HSI (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  pxOp2 f (HSI r1 g1 b1) (HSI r2 g2 b2) = HSI (f r1 r2) (f g1 g2) (f b1 b2)
  {-# INLINE pxOp2 #-}

  strongest (HSI r g b)                 = pixel . maximum $ [r, g, b]
  {-# INLINE strongest #-}

  weakest (HSI r g b)                   = pixel . minimum $ [r, g, b]
  {-# INLINE weakest #-}


instance Num HSI where
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


instance Fractional HSI where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance Floating HSI where
  pi      = pixel pi
  {-# INLINE pi #-}

  exp     = pxOp exp
  {-# INLINE exp #-}

  log     = pxOp log
  {-# INLINE log #-}

  sin     = pxOp sin
  {-# INLINE sin #-}

  cos     = pxOp cos
  {-# INLINE cos #-}

  asin    = pxOp asin
  {-# INLINE asin #-}

  atan    = pxOp atan
  {-# INLINE atan #-}

  acos    = pxOp acos
  {-# INLINE acos #-}

  sinh    = pxOp sinh
  {-# INLINE sinh #-}

  cosh    = pxOp cosh
  {-# INLINE cosh #-}

  asinh   = pxOp asinh
  {-# INLINE asinh #-}

  atanh   = pxOp atanh
  {-# INLINE atanh #-}

  acosh   = pxOp acosh
  {-# INLINE acosh #-}


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


zipRGB :: RGB -> (Double, Double, Double)
{-# INLINE zipRGB #-}  
zipRGB (RGB r g b) = (r,g,b)


unzipRGB :: (Double, Double, Double) -> RGB
{-# INLINE unzipRGB #-}
unzipRGB (r,g,b) = (RGB r g b)


zipHSI :: HSI -> (Double, Double, Double)
{-# INLINE zipHSI #-}  
zipHSI (HSI r g b) = (r,g,b)


unzipHSI :: (Double, Double, Double) -> HSI
{-# INLINE unzipHSI #-}
unzipHSI (r,g,b) = (HSI r g b)


derivingUnbox "RGBPixel"
    [t| (Unbox Double) => RGB -> (Double, Double, Double) |]
    [| zipRGB |]
    [| unzipRGB |]


derivingUnbox "HSIPixel"
    [t| (Unbox Double) => HSI -> (Double, Double, Double) |]
    [| zipHSI |]
    [| unzipHSI |]


                               
{-
instance Convertable (Image Gray, Image Gray, Image Gray) (Image HSI) where
  convert imgs@(h, s, i) = fromVector m n $ V.zipWith3 HSI (f h) (f s) (f i)
    where (m, n) = dims h
          f = V.map (\(Gray d) -> d) . toVector
-}

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
