{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, GADTs,
TypeFamilies, MultiParamTypeClasses, NoMonomorphismRestriction,
UndecidableInstances, ViewPatterns #-}

module HIP.Complex.Pixel (
  Complex (..),
  mag, arg, conj, real, imag, fromPol, toRect,
  ComplexInner (..)
  ) where

import Prelude hiding (map, zipWith)
import HIP.Interface (Pixel(..))


{- | Every instance of this ComplexInner class can be used as a real and imaginary
parts of a Complex pixel. -}
class (Floating (Inner px), Fractional (Inner px),
       Floating px, Fractional px, Pixel px) =>
      ComplexInner px where
  apply2c :: [(Inner px -> Inner px -> (Inner px, Inner px))] -> px -> px -> Complex px


infix  6  :+:

data Complex px where
  (:+:) :: ComplexInner px => !px -> !px -> Complex px 
  --(:*:) :: ComplexInner px => !px -> !px -> Complex px 


instance Eq (Complex px) where
  (==) !(px1x :+: px1y) !(px2x :+: px2y) = px1x == px2x && px1y == px2y
  --(==) !(px1r :*: px1t) !(px2r :*: px2t) = px1r == px2r && px1t == px2t


-- | Magnitude (or modulus, or radius)
mag :: (ComplexInner px) => Complex px -> px
mag !(pxReal :+: pxImag) = sqrt (pxReal ^ (2 :: Int) + pxImag ^ (2 :: Int))
{-# INLINE mag #-}


-- | The principal value of Argument of a Complex pixel (or Phase).
arg :: (ComplexInner px) => Complex px -> px
arg !(pxX :+: pxY) = apply2 (repeat f) pxX pxY where
  f !x !y | x /= 0          = atan (y / x) + (pi / 2) * (1 - signum x)
          | x == 0 && y /=0 = (pi / 2) * signum y
          | otherwise = 0
  {-# INLINE f #-}
{-# INLINE arg #-}


-- | Create a complex pixel from two real pixels, which represent a magnitude
-- and an argument, ie. radius and phase
fromPol :: (ComplexInner px) => px -> px -> Complex px
fromPol !r !theta = (r * cos theta) :+: (r * sin theta)
{-# INLINE fromPol #-}

toRect :: (ComplexInner px) => Complex px -> (px, px)
toRect !(px1 :+: px2) = (px1, px2)
{-# INLINE toRect #-}


{- | Conjugate a complex pixel -}
conj :: (ComplexInner px) => Complex px -> Complex px
conj !(x :+: y) = x :+: (-y)
{-# INLINE conj #-}


{- | Extracts a real part from a complex pixel -}
real :: (ComplexInner px) => Complex px -> px
real !(px :+: _) = px
{-# INLINE real #-}


{-| Extracts an imaginary part of a pixel -}
imag :: (ComplexInner px) => Complex px -> px
imag !(_  :+: px) = px
{-# INLINE imag #-}


pxOp :: (ComplexInner px) => (px -> px) -> (Complex px) -> (Complex px)
pxOp !op !(px1 :+: px2) = op px1 :+: op px2
{-# INLINE pxOp #-}

pxOp2 :: (ComplexInner px) => (px -> px -> px) -> (Complex px) -> (Complex px) -> (Complex px)
pxOp2 !op !(px1 :+: px2) (px1' :+: px2') = op px1 px1' :+: op px2 px2'
{-# INLINE pxOp2 #-}


instance ComplexInner px => Pixel (Complex px) where
  type Inner (Complex px) = Inner px

  pixel !v = (pixel v) :+: (pixel v)
  {-# INLINE pixel #-}
  
  arity (px1 :+: px2) = arity px1 + arity px2
  {-# INLINE arity #-}

  ref !n !px@(px1 :+: px2) =
    if n < 0 || n >= arity px
    then error ("Referencing "++show n++"is out of bounds for "++showType px)
    else if n < arity1 then ref n px1 else ref (n-arity1) px2
    where !arity1 = arity px1
  {-# INLINE ref #-}
  
  apply !fs !(px1 :+: px2) = apply fs1 px1 :+: apply fs2 px2
    where !(fs1, fs2) = splitAt (arity px1) fs
  {-# INLINE apply #-}

  apply2 !fs !(px1a :+: px2a) !(px1b :+: px2b) =
    apply2 fs1 px1a px1b :+: apply2 fs2 px2a px2b
    where !(fs1, fs2) = splitAt (arity px1a) fs
  {-# INLINE apply2 #-}

  showType (!px :+: _) = "Complex "++(showType px)
  {-# INLINE showType #-}
  


instance (ComplexInner px) => Num (Complex px) where
  (+) = pxOp2 (+)
  {-# INLINE (+) #-}
  
  (-) = pxOp2 (-)
  {-# INLINE (-) #-}
  
  (*) !(x :+: y) !(x' :+: y') = (x*x' - y*y') :+: (x*y' + y*x')
  {-# INLINE (*) #-}

  negate = pxOp negate
  {-# INLINE negate #-}
  
  abs !z = (mag z) :+: (fromInteger 0)
  {-# INLINE abs #-}
  
  signum !z@(x :+: _)
    | mag' == 0 = (fromInteger 0) :+: (fromInteger 0)
    | otherwise = (x / mag') :+: (x / mag')
    where mag' = mag z
  {-# INLINE signum #-}

  fromInteger n = nd :+: nd where nd = fromInteger n
  {-# INLINE fromInteger #-}


instance ComplexInner px => Fractional (Complex px) where
  (/) !(x :+: y) !(x' :+: y') =
    ((x*x' + y*y') / mag2) :+: ((y*x' - x*y') / mag2) where
      !mag2 = x'*x' + y'*y'
  {-# INLINE (/) #-}
  
  recip          = pxOp recip
  {-# INLINE recip #-}
  
  fromRational !n = nd :+: nd where nd = fromRational n
  {-# INLINE fromRational #-}


instance ComplexInner px => Floating (Complex px) where
  pi             =  pi :+: 0
  {-# INLINE pi #-}
  
  exp !(x :+: y)    =  (expX * cos y) :+: (expX * sin y)
    where !expX = exp x
  {-# INLINE exp #-}
  
  log !z          =  (log (mag z)) :+: (arg z)
  {-# INLINE log #-}
    --sqrt (0:+:0)    =  0
    {-
    sqrt z@(x:+:y)  =  u :+: (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)
    -}
  sin !(x:+:y)     =  (sin x * cosh y) :+: (cos x * sinh y)
  {-# INLINE sin #-}
  
  cos !(x:+:y)     =  (cos x * cosh y) :+: (- sin x * sinh y)
  {-# INLINE cos #-}
  
  tan !(x:+:y)     =  ((sinx * coshy) :+: (cosx * sinhy)) /
                      ((cosx * coshy) :+: (-sinx * sinhy))
    where !sinx  = sin x
          !cosx  = cos x
          !sinhy = sinh y
          !coshy = cosh y
  {-# INLINE tan #-}

  sinh !(x:+:y)    =  (cos y * sinh x) :+: (sin  y * cosh x)
  {-# INLINE sinh #-}
  
  cosh !(x:+:y)    =  (cos y * cosh x) :+: (sin y * sinh x)
  {-# INLINE cosh #-}
  
  tanh !(x:+:y)    =  ((cosy * sinhx) :+: (siny * coshx)) /
                      ((cosy * coshx) :+: (siny * sinhx))
    where !siny  = sin y
          !cosy  = cos y
          !sinhx = sinh x
          !coshx = cosh x
  {-# INLINE tanh #-}

  asin !z@(x :+: y)  =  y' :+: (-x')
    where !(x' :+: y') = log (((-y) :+: x) + sqrt (1 - z * z))
  {-# INLINE asin #-}
           
  acos !z         =  y'' :+: (-x'')
    where !(x'' :+: y'')  = log (z + ((-y') :+: x'))
          !(x'  :+: y' )  = sqrt (1 - z * z)
  {-# INLINE acos #-}
  
  atan !z@(x :+: y) =  y' :+: (-x')
    where !(x' :+: y') = log (((1 - y) :+: x) / sqrt (1 + z * z))
  {-# INLINE atan #-}

  asinh !z        =  log (z + sqrt (1+z*z))
  {-# INLINE asinh #-}
  
  acosh !z        =  log (z + (z + 1) * sqrt ((z - 1) / (z + 1)))
  {-# INLINE acosh #-}
  
  atanh !z        =  0.5 * log ((1 + z) / (1 - z))
  {-# INLINE atanh #-}

    
instance Show px => Show (Complex px) where
  show (px1 :+: px2) = "(" ++show px1 ++" + i" ++show px2 ++")"


