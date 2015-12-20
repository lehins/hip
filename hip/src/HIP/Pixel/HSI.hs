{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, 
             TypeFamilies, UndecidableInstances #-}

module HIP.Pixel.HSI (
  HSI (..)
  ) where

import Data.Data
import HIP.Pixel.Base (Pixel(..))

-- | HSI colorspace model consists of Hue, Saturation and Intensity. When read
-- from file or converted from a normalized image in other colorspace values
-- will be in ranges:
-- * Hue: @H = [0, 2*pi)@
-- * Saturation: @S = [0, 1]@
-- * Intensity: @I = [0, 1]@
data HSI = HSI { hue :: {-# UNPACK #-} !Double
               , sat :: {-# UNPACK #-} !Double
               , int :: {-# UNPACK #-} !Double
               } deriving (Typeable, Data, Eq)


pxOp :: (Double -> Double) -> HSI -> HSI
pxOp !f (HSI h s i) = HSI (f h) (f s) (f i)
{-# INLINE pxOp #-}

pxOp2 :: (Double -> Double -> Double) -> HSI -> HSI -> HSI
pxOp2 !f (HSI h1 s1 i1) (HSI h2 s2 i2) = HSI (f h1 h2) (f s1 s2) (f i1 i2)
{-# INLINE pxOp2 #-}


instance Pixel HSI where
  type Channel HSI = Double

  fromDouble !d = HSI d d d
  {-# INLINE fromDouble #-}

  arity _ = 3
  {-# INLINE arity #-}

  ref !px 0 = hue px
  ref !px 1 = sat px
  ref !px 2 = int px
  ref n px = error ("Referencing "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE ref #-}

  update !px 0 h = px { hue = h }
  update !px 1 s = px { sat = s }
  update !px 2 i = px { int = i }
  update !px n _ = error ("Updating "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE update #-}

  apply (f1:f2:f3:_) (HSI h s i) = HSI (f1 h) (f2 s) (f3 i)
  apply _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply #-}

  apply2 (f1:f2:f3:_) (HSI h1 s1 i1) (HSI h2 s2 i2) = HSI (f1 h1 h2) (f2 s1 s2) (f3 i1 i2)
  apply2 _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2 #-}

  maxChannel (HSI h s i) = max (max h s) i
  {-# INLINE maxChannel #-}

  minChannel (HSI h s i) = min (min h s) i
  {-# INLINE minChannel #-}
  

instance Num HSI where
  (+)         = pxOp2 (+)
  {-# INLINE (+) #-}
  
  (-)         = pxOp2 (-)
  {-# INLINE (-) #-}
  
  (*)         = pxOp2 (*)
  {-# INLINE (*) #-}
  
  abs         = pxOp abs
  {-# INLINE abs #-}
  
  signum      = pxOp signum
  {-# INLINE signum #-}
  
  fromInteger = fromDouble . fromIntegral 
  {-# INLINE fromInteger #-}


instance Fractional HSI where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}

  recip        = pxOp recip
  {-# INLINE recip #-}
  
  fromRational = fromDouble . fromRational 
  {-# INLINE fromRational #-}


instance Floating HSI where
  pi    = fromDouble pi
  {-# INLINE pi #-}
  
  exp   = pxOp exp
  {-# INLINE exp #-}
  
  log   = pxOp log
  {-# INLINE log #-}
  
  sin   = pxOp sin
  {-# INLINE sin #-}
  
  cos   = pxOp cos
  {-# INLINE cos #-}
  
  asin  = pxOp asin
  {-# INLINE asin #-}
  
  atan  = pxOp atan
  {-# INLINE atan #-}
  
  acos  = pxOp acos
  {-# INLINE acos #-}
  
  sinh  = pxOp sinh
  {-# INLINE sinh #-}
  
  cosh  = pxOp cosh
  {-# INLINE cosh #-}
  
  asinh = pxOp asinh
  {-# INLINE asinh #-}
  
  atanh = pxOp atanh
  {-# INLINE atanh #-}
  
  acosh = pxOp acosh
  {-# INLINE acosh #-}


instance Ord HSI where
  compare (HSI h1 s1 i1) (HSI h2 s2 i2) = compare (h1, s1, i1) (h2, s2, i2)
  {-# INLINE compare #-}

instance Show HSI where
  show (HSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"
  
