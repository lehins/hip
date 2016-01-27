{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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
  type PixelElt cs e :: * -- | Representation of a pixel, such that it can be
                          -- an element of any Array
  data Pixel cs e

  rank :: Num Ix => cs -> Ix

  fromChannel :: e -> Pixel cs e

  fromElt :: PixelElt cs e -> Pixel cs e

  toElt :: Pixel cs e -> PixelElt cs e

  pixelRank :: Num Ix => Pixel cs e -> Ix
  pixelRank _ = rank (undefined :: cs)

  -- | cond -> eq -> ...
  getEltCh :: (Show Ix, Num Ix) =>
              IfElseThen b e -> Equals Ix b -> PixelElt cs e -> cs -> Ix -> e

  getPxCh :: (Show Ix, Num Ix) => IfElseThen b e -> Equals Ix b -> Pixel cs e -> Ix -> e
  
  chOp :: Num Ix => (Ix -> e -> e) -> Pixel cs e -> Pixel cs e 

  chOp2 :: Num Ix => (Ix -> e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e
  
  pxOp :: (e -> e) -> Pixel cs e -> Pixel cs e

  pxOp2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e

  indexElt' :: (Array arr cs e, Num Ix) => Image arr cs e -> cs -> (Ix, Ix) -> PixelElt cs e
                                 
  

class (ColorSpace cs, Num (Pixel cs e), Num e, Num Ix, Eq Ix, Elt cs e) =>
      Array arr cs e where
  type Ix :: *
  type S c :: *
  type Elt cs e :: Constraint
  
  data Image arr cs e

  dims :: Image arr cs e -> (Ix, Ix)

  index :: Image arr cs e -> Ix -> (Ix, Ix) -> e

  indexPx :: Image arr cs e -> (Ix, Ix) -> Pixel cs e

  indexElt :: Image arr cs e -> (Ix, Ix) -> PixelElt cs e
                                 
  make :: (Ix, Ix) -> ((Ix, Ix) -> Pixel cs e) -> Image arr cs e

  singleton :: Pixel cs e -> Image arr cs e

  map :: (e -> e) -> Image arr cs e -> Image arr cs e

  mapCh :: (Ix -> e -> e) -> Image arr cs e -> Image arr cs e

  mapPx :: (Elt cs' e', e' ~ S c') =>
           (Pixel cs' e' -> Pixel cs e)
        -> Image arr cs' e'
        -> Image arr cs  e

  mapElt :: (Elt cs' e', e' ~ S c') =>
            (PixelElt cs' e' -> PixelElt cs e)
         -> Image arr cs' e' -> Image arr cs e

  zipWith :: (e -> e -> e) -> Image arr cs e -> Image arr cs e -> Image arr cs e
  
  zipWithElt :: (Elt cs1 e1, Elt cs2 e2, e1 ~ S c1, e2 ~ S c2) =>
                (PixelElt cs1 e1 -> PixelElt cs2 e2 -> PixelElt cs e)
             -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e
               
  zipWithPx :: (Elt cs1 e1, Elt cs2 e2, e1 ~ S c1, e2 ~ S c2) =>
               (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
            -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  traverse :: Elt cs e =>
              Image arr cs e
           -> ((Ix, Ix) -> (Ix, Ix))
           -> (((Ix, Ix) -> e) -> (Ix, Ix) -> e)
           -> Image arr cs e

  traverseCh :: Elt cs e =>
                Image arr cs e
             -> ((Ix, Ix) -> (Ix, Ix))
             -> ((Ix -> (Ix, Ix) -> e) -> Ix -> (Ix, Ix) -> e)
             -> Image arr cs e

  traversePx :: (Elt cs' e', e' ~ S c') =>
                Image arr cs' e'
             -> ((Ix, Ix) -> (Ix, Ix))
             -> (((Ix, Ix) -> Pixel cs' e') -> (Ix, Ix) -> Pixel cs e)
             -> Image arr cs e
  
  group :: Image arr cs e -> Image arr cs e

  disperse :: Image arr cs e -> Image arr cs e

  compute :: Strategy str => str -> Image arr cs e -> a --Image arr cs e

  computeM :: (Monad m, Strategy str) => str -> Image arr' cs e' -> m (Image arr cs e)

  

class Strategy str where


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


instance (Show Ix, Array arr cs e) => Show (Image arr cs e) where
  show ((dims -> (m, n)) :: Image arr cs e) =
    "<Image "++(show (undefined :: cs))++": "++(show m)++"x"++(show n)++">"





