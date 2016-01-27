{-# LANGUAGE ConstraintKinds, GADTs, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Repa.Internal (
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface

import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Array.Repa hiding (Array, map, zipWith, rank, index, traverse, (++))
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R (Elt)

import Graphics.Image.ColorSpace.RGB


data R -- RepaArray

data Repr = RU -- Repa Unboxed representation
          | RD -- Repa Delayed representation


cond b t1 t2 = if b then t1 else t2


instance Elt cs e => Array R cs e where
  type Ix = Int
  type S e' = e'
  type Elt cs e = (ColorSpace cs, 
                   R.Elt e, Unbox e, Num e,
                   R.Elt (PixelElt cs e), Unbox (PixelElt cs e))

  data Image R cs e where
    RImageSc :: R.Array U DIM1 e                       -> Image R cs e
    RImageCh :: Source r e =>
                Repr -> R.Array r DIM3 e               -> Image R cs e
    RImagePx :: Source r (PixelElt cs e) =>
                Repr -> R.Array r DIM2 (PixelElt cs e) -> Image R cs e

  dims (RImageSc   _                             ) = (1, 1)
  dims (RImageCh _ (extent -> (Z :. _ :. m :. n))) = (m, n)
  dims (RImagePx _ (extent -> (     Z :. m :. n))) = (m, n)

  index (RImageSc   arr) c _      = R.index arr (Z :. c)
  index (RImageCh _ arr) c (i, j) = R.index arr (Z :. c :. i :. j) 
  index (RImagePx _ arr) c (i, j) =
    getEltCh cond (==) (R.index arr (Z :. i :. j)) (undefined :: cs) c

  indexPx img              ix     = fromElt $ indexElt img ix

  indexElt (RImagePx _ arr) (i, j) = R.index arr (Z :. i :. j)
  indexElt img              ix     = indexElt' img (undefined :: cs) ix

  singleton px = RImageSc . computeS $ fromFunction (Z :. pixelRank px) getCh' where
    getCh' (Z :. c) = getPxCh cond (==) px c

  make (m, n) f = RImagePx RD $ fromFunction (Z :. m :. n) getPx' where
    getPx' (Z :. i :. j) = toElt $ f (i, j)

  map f (RImageSc   arr) = RImageSc . computeS $ R.map f arr
  map f (RImageCh _ arr) = RImageCh RD $ R.map f arr
  map f img              = map f (disperse img)

  mapPx f (RImagePx _ arr) = RImagePx RD $ R.map (toElt . f . fromElt) arr
  mapPx f img              = mapPx f (group img)

  mapCh f (RImageCh _ arr :: Image R cs e) = RImageCh RD $ R.traverse arr id f'
    where f' getCh' sh@(Z :. c :. _ :. _) = f c (getCh' sh)
  mapCh f img = mapCh f (disperse img)

  zipWith f (RImageSc   arr1)   (RImageSc   arr2) = RImageSc . computeS $ R.zipWith f arr1 arr2
  zipWith f (RImageCh _ arr1)   (RImageCh _ arr2) = RImageCh RD $ R.zipWith f arr1 arr2
  zipWith f (RImageSc   arr1)   (RImageCh _ arr2) = RImageCh RD $ R.traverse arr2 id f'
    where f' getCh' sh@(Z :. c  :. _ :. _) = f (R.index arr1 (Z :. c)) (R.index arr2 sh)
  zipWith f img1@(RImageCh _ _) img2@(RImageSc _) = zipWith (flip f) img2 img1
  zipWith f img1                img2              = zipWith f (disperse img1) (disperse img2)

  zipWithElt f img1@(RImageSc _)   img2@(RImageSc _)   =
    singleton $ fromElt $ f (indexElt img1 (0, 0)) (indexElt img2 (0, 0))
  zipWithElt f img1@(RImageSc _)   img2@(RImagePx _ _) =
    mapElt (f (indexElt img1 (0, 0))) img2
  zipWithElt f img1@(RImagePx _ _) img2@(RImageSc _)   =
    mapElt (`f` (indexElt img2 (0, 0))) img1
  zipWithElt f (RImagePx _ arr1)   (RImagePx _ arr2)   =
    RImagePx RU $ R.zipWith f arr1 arr2
  zipWithElt f img1                img2                =
    zipWithElt f (group img1) (group img2)
  
  zipWithPx f = zipWithElt func where
    func elt1 elt2 = toElt $ f (fromElt elt1) (fromElt elt2)

  traverse (RImageCh _ arr)   newDims newCh =
    RImageCh RD $ R.traverse arr newDims' newCh' where
      newDims' (Z :. c :. m :. n) =
        let (m', n') = newDims (m, n) in (Z :. c :. m' :. n')
      newCh' getCh' (Z :. c :. i :. j) =
        let getCh'' (i', j') = getCh' (Z :. c :. i' :. j') in newCh getCh'' (i, j)
  traverse img@(RImagePx _ _) newDims newCh = traverse (disperse img) newDims newCh
  traverse (RImageSc _)       _       _     = error _error_traverse_scalar

  traverseCh (RImageCh _ arr)   newDims newCh =
    RImageCh RD $ R.traverse arr newDims' newCh' where
      newDims' (Z :. c :. m :. n) = uncurry (ix3 c) $ newDims (m, n)
      newCh' getCh' (Z :. c :. i :. j) =
        let getCh'' c' (i', j') = getCh' (Z :. c' :. i' :. j') in newCh getCh'' c (i, j)
  traverseCh img@(RImagePx _ _) newDims newCh = traverseCh (disperse img) newDims newCh
  traverseCh (RImageSc _)       _       _     = error _error_traverse_scalar

  traversePx (RImagePx _ arr)   newDims newPx =
    RImagePx RD $ R.traverse arr newDims' newElt where
      newDims' (Z :. m :. n) = uncurry ix2 $ newDims (m, n)
      newElt getElt (Z :. i :. j) = let getPx (i', j') = fromElt $ getElt (Z :. i' :. j')
                                    in toElt $ newPx getPx (i, j)        
  traversePx img@(RImageCh _ _) newDims newPx = traversePx (group img) newDims newPx
  traversePx (RImageSc _)       _       _     = error _error_traverse_scalar
    
  group img@(RImageCh _ (extent -> (Z :. _ :. m :. n))) =
    RImagePx RD $ fromFunction (Z :. m :. n) getElt' where
      getElt' (Z :. i :. j) = indexElt' img (undefined :: cs) (i, j)
  group img = img
  
  disperse (RImagePx _ arr) = RImageCh RD $ R.traverse arr getSh' getCh' where
    getSh' (Z :. m :. n) = Z :. (rank (undefined :: cs)) :. m :. n
    getCh' getPx' (Z :. c :. i :. j) =
      getEltCh cond (==) (getPx' (Z :. i :. j)) (undefined :: cs) c
  disperse img = img


_error_traverse_scalar :: String
_error_traverse_scalar =
  "Traversal of a scalar image does not make sense, hence it is not implemented."
