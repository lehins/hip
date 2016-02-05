{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Interface (
  ColorSpace(..), Alpha(..), Array(..), ManifestArray(..), Convertible(..)
  ) where

import Prelude hiding (map, zipWith)
import GHC.Exts (Constraint)


class (ColorSpace (Opaque cs), ColorSpace cs) => Alpha cs where
  type Opaque cs
  
  getAlpha :: Pixel cs e -> e

  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e
  
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


class (Enum cs, Show cs) => ColorSpace cs where
  -- | Representation of a pixel, such that it can be an element of any Array
  type PixelElt cs e

  data Pixel cs e

  fromChannel :: e -> Pixel cs e

  fromElt :: PixelElt cs e -> Pixel cs e

  toElt :: Pixel cs e -> PixelElt cs e

  getPxCh :: Pixel cs e -> cs -> e
  
  chOp :: (cs -> e -> e) -> Pixel cs e -> Pixel cs e 

  chOp2 :: (cs -> e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e
  
  pxOp :: (e -> e) -> Pixel cs e -> Pixel cs e

  pxOp2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e

                                 
  
class Convertible a b where
  convert :: a -> b



class Array arr cs e => ManifestArray arr cs e where

  (|*|) :: Image arr cs e -> Image arr cs e -> Image arr cs e

  deepSeqImage :: Image arr cs e -> Image arr cs e



class (Show arr, ColorSpace cs, Num (Pixel cs e), Num e, Elt arr cs e) =>
      Array arr cs e where
  
  type Elt arr cs e :: Constraint
  type Elt arr cs e = ()
  
  data Image arr cs e

  dims :: Image arr cs e -> (Int, Int)

  index :: Image arr cs e -> (Int, Int) -> Pixel cs e

  make :: (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e

  singleton :: Pixel cs e -> Image arr cs e

  map :: Elt arr cs' e' =>
         (Pixel cs' e' -> Pixel cs e)
      -> Image arr cs' e'
      -> Image arr cs  e

  imap :: Array arr cs' e' =>
          ((Int, Int) -> Pixel cs' e' -> Pixel cs e)
       -> Image arr cs' e' -> Image arr cs e

  zipWith :: (Elt arr cs1 e1, Elt arr cs2 e2) =>
             (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
          -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  traverse :: Array arr cs' e' =>
              Image arr cs' e'
           -> ((Int, Int) -> (Int, Int))
           -> (((Int, Int) -> Pixel cs' e') -> (Int, Int) -> Pixel cs e)
           -> Image arr cs e
  
  transpose :: Image arr cs e -> Image arr cs e

  backpermute :: (Int, Int) -> ((Int, Int) -> (Int, Int)) -> Image arr cs e -> Image arr cs e



instance (ColorSpace cs, Num e) => Num (Pixel cs e) where
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
  
  fromInteger = fromChannel . fromInteger
  {-# INLINE fromInteger#-}
  

instance (ColorSpace cs, Fractional e) => Fractional (Pixel cs e) where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}
  
  recip        = pxOp recip
  {-# INLINE recip #-}

  fromRational = fromChannel . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs, Floating e) => Floating (Pixel cs e) where
  pi      = fromChannel pi
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


  
instance Array arr cs e => Num (Image arr cs e) where
  (+)         = zipWith (+)
  {-# INLINE (+) #-}
  
  (-)         = zipWith (-)
  {-# INLINE (-) #-}
  
  (*)         = zipWith (*)
  {-# INLINE (*) #-}
  
  abs         = map abs
  {-# INLINE abs #-}
  
  signum      = map signum
  {-# INLINE signum #-}
  
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger#-}


instance (Fractional (Pixel cs e), Fractional e, Array arr cs e) =>
         Fractional (Image arr cs e) where
  (/)          = zipWith (/)
  {-# INLINE (/) #-}
  
  fromRational = singleton . fromRational 
  {-# INLINE fromRational #-}


instance (Floating (Pixel cs e), Floating e, Array arr cs e) =>
         Floating (Image arr cs e) where
  pi    = singleton pi
  {-# INLINE pi #-}
  
  exp   = map exp
  {-# INLINE exp #-}
  
  log   = map log
  {-# INLINE log#-}
  
  sin   = map sin
  {-# INLINE sin #-}
  
  cos   = map cos
  {-# INLINE cos #-}
  
  asin  = map asin
  {-# INLINE asin #-}
  
  atan  = map atan
  {-# INLINE atan #-}
  
  acos  = map acos
  {-# INLINE acos #-}
  
  sinh  = map sinh
  {-# INLINE sinh #-}
  
  cosh  = map cosh
  {-# INLINE cosh #-}
  
  asinh = map asinh
  {-# INLINE asinh #-}
  
  atanh = map atanh
  {-# INLINE atanh #-}
  
  acosh = map acosh
  {-# INLINE acosh #-}  


instance Array arr cs e => Show (Image arr cs e) where
  show ((dims -> (m, n)) :: Image arr cs e) =
    "<Image "++(show (undefined :: arr))++" "++(show (undefined :: cs))++
    ": "++(show m)++"x"++(show n)++">"

