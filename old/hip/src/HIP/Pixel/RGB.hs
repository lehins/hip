{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses,
             TypeFamilies, UndecidableInstances #-}

module HIP.Pixel.RGB (
  RGB (..)
  ) where

import Data.Data
import HIP.Pixel.Base (Pixel(..))

data RGB = RGB { red   :: {-# UNPACK #-} !Double
               , green :: {-# UNPACK #-} !Double
               , blue  :: {-# UNPACK #-} !Double
               } deriving (Typeable, Data, Eq)

pxOp :: (Double -> Double) -> RGB -> RGB
pxOp !f (RGB r g b) = RGB (f r) (f g) (f b)
{-# INLINE pxOp #-}

pxOp2 :: (Double -> Double -> Double) -> RGB -> RGB -> RGB
pxOp2 !f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
{-# INLINE pxOp2 #-}


instance Pixel RGB where
  type Channel RGB = Double
  
  fromDouble !d = RGB d d d
  {-# INLINE fromDouble #-}

  arity _ = 3
  {-# INLINE arity #-}

  ref !px 0 = red px
  ref !px 1 = green px
  ref !px 2 = blue px
  ref !px n = error ("Referencing "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE ref #-}

  update !px 0 r = px { red   = r }
  update !px 1 g = px { green = g }
  update !px 2 b = px { blue  = b }
  update !px n _ = error ("Updating "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE update #-}

  apply (f1:f2:f3:_) (RGB r g b) = RGB (f1 r) (f2 g) (f3 b)
  apply _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply #-}

  apply2 (f1:f2:f3:_) (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f1 r1 r2) (f2 g1 g2) (f3 b1 b2)
  apply2 _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2 #-}

  maxChannel (RGB r g b) = max (max r g) b
  {-# INLINE maxChannel #-}

  minChannel (RGB r g b) = min (min r g) b
  {-# INLINE minChannel #-}

  fromChannel = fromDouble
  {-# INLINE fromChannel #-}


instance Num RGB where
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


instance Fractional RGB where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}
  
  recip        = pxOp recip
  {-# INLINE recip #-}

  fromRational = fromDouble . fromRational
  {-# INLINE fromRational #-}


instance Floating RGB where
  pi      = fromDouble pi
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


instance Ord RGB where
  compare (RGB r1 g1 b1) (RGB r2 g2 b2) = compare (r1, g1, b1) (r2, g2, b2)
  {-# INLINE compare #-}

instance Show RGB where
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"
