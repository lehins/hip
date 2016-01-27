{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, TypeFamilies, ViewPatterns #-}

module Graphics.Image.Interface (
  ColorSpace(..), Array(..)
  ) where

import Prelude hiding (map, zipWith)
import GHC.Exts (Constraint)

type IfElseThen b t = b -> t -> t -> t

type Equals t b = t -> t -> b


class Show cs => ColorSpace cs where
  -- | Representation of a pixel, such that it can be an element of any Array
  type PixelElt cs e

  data Pixel cs e

  rank :: Num ix => cs -> ix

  fromChannel :: e -> Pixel cs e

  fromElt :: PixelElt cs e -> Pixel cs e

  toElt :: Pixel cs e -> PixelElt cs e

  pixelRank :: Num ix => Pixel cs e -> ix
  pixelRank _ = rank (undefined :: cs)

  getEltCh :: (Show ix, Num ix) =>
              IfElseThen b e -> Equals ix b -> PixelElt cs e -> cs -> ix -> e

  getPxCh :: (Show ix, Num ix) => IfElseThen b e -> Equals ix b -> Pixel cs e -> ix -> e
  
  chOp :: Num ix => (ix -> e -> e) -> Pixel cs e -> Pixel cs e 

  chOp2 :: Num ix => (ix -> e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e
  
  pxOp :: (e -> e) -> Pixel cs e -> Pixel cs e

  pxOp2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e

  indexElt' :: (Array arr cs e, ix ~ Ix arr, Num ix) =>
               Image arr cs e -> cs -> (ix, ix) -> PixelElt cs e
                                 
  

class (ColorSpace cs, Num (Pixel cs e), Num e, Num (Ix arr), Eq (Ix arr)) =>
      Array arr cs e where
  type Ix arr :: *
  type S arr c :: *
  type S arr c = c
  
  type Elt arr cs e :: Constraint
  type Elt arr cs e = ()
  
  data Image arr cs e

  dims :: Image arr cs e -> (Ix arr, Ix arr)

  index :: Image arr cs e -> Ix arr -> (Ix arr, Ix arr) -> e

  indexPx :: Image arr cs e -> (Ix arr, Ix arr) -> Pixel cs e

  indexElt :: Image arr cs e -> (Ix arr, Ix arr) -> PixelElt cs e
                                 
  make :: (Ix arr, Ix arr) -> ((Ix arr, Ix arr) -> Pixel cs e) -> Image arr cs e

  singleton :: Pixel cs e -> Image arr cs e

  -- | Map a function over each pixel's channel in source image.
  map :: (e -> e) -- ^ A function that takes pixel's channel value from a source
                  -- image and returns pixel's new channel value at the same
                  -- location for the result image.
      -> Image arr cs e -- ^ Source image.
      -> Image arr cs e -- ^ Result image.

  mapElt :: (Elt arr cs' e', e' ~ S arr c') =>
            (PixelElt cs' e' -> PixelElt cs e)
         -> Image arr cs' e' -> Image arr cs e

  mapPx :: (Elt arr cs' e', e' ~ S arr c') =>
           (Pixel cs' e' -> Pixel cs e)
        -> Image arr cs' e'
        -> Image arr cs  e

  imap :: ((Ix arr, Ix arr) -> Ix arr -> e -> e) -> Image arr cs e -> Image arr cs e

  imapElt :: (Elt arr cs' e', e' ~ S arr c') =>
             ((Ix arr, Ix arr) -> PixelElt cs' e' -> PixelElt cs e)
          -> Image arr cs' e' -> Image arr cs e

  imapPx :: (Elt arr cs' e', e' ~ S arr c') =>
            ((Ix arr, Ix arr) -> PixelElt cs' e' -> PixelElt cs e)
         -> Image arr cs' e' -> Image arr cs e

  zipWith :: (e -> e -> e) -> Image arr cs e -> Image arr cs e -> Image arr cs e
  
  zipWithElt :: (Elt arr cs1 e1, Elt arr cs2 e2, e1 ~ S arr c1, e2 ~ S arr c2) =>
                (PixelElt cs1 e1 -> PixelElt cs2 e2 -> PixelElt cs e)
             -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e
               
  zipWithPx :: (Elt arr cs1 e1, Elt arr cs2 e2, e1 ~ S arr c1, e2 ~ S arr c2) =>
               (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
            -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  traverse :: Image arr cs e
           -> ((Ix arr, Ix arr) -> (Ix arr, Ix arr))
           -> (((Ix arr, Ix arr) -> e) -> (Ix arr, Ix arr) -> e)
           -> Image arr cs e

  traverseCh :: Image arr cs e
             -> ((Ix arr, Ix arr) -> (Ix arr, Ix arr))
             -> ((Ix arr -> (Ix arr, Ix arr) -> e) -> Ix arr -> (Ix arr, Ix arr) -> e)
             -> Image arr cs e

  traversePx :: (Elt arr cs' e', e' ~ S arr c') =>
                Image arr cs' e'
             -> ((Ix arr, Ix arr) -> (Ix arr, Ix arr))
             -> (((Ix arr, Ix arr) -> Pixel cs' e') -> (Ix arr, Ix arr) -> Pixel cs e)
             -> Image arr cs e
  
  group :: Image arr cs e -> Image arr cs e

  disperse :: Image arr cs e -> Image arr cs e



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


instance (Show (Ix arr), Array arr cs e) => Show (Image arr cs e) where
  show ((dims -> (m, n)) :: Image arr cs e) =
    "<Image "++(show (undefined :: cs))++": "++(show m)++"x"++(show n)++">"





