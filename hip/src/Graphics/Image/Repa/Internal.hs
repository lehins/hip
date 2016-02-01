{-# LANGUAGE ConstraintKinds, GADTs, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Repa.Internal (
  RD, RP, RS
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface

import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Array.Repa hiding (
  Array, map, zipWith, rank, index, traverse, (++), computeP, computeS)
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R (Elt, suspendedComputeP)

import Graphics.Image.ColorSpace.RGB

-- | Repa Delayed Array representation.
data RD

-- | Repa Unboxed Array representation, which is computed in parallel.
data RP

-- | Repa Unboxed Array representation, which is computed sequentially. 
data RS
  

delayImage :: (Unbox (PixelElt cs e), Unbox e) =>
              Image RD cs e -> Image RD cs e
delayImage (RUImagePx arr) = RDImagePx . R.delay $ arr
delayImage (RUImageCh arr) = RDImageCh . R.delay $ arr
delayImage img             = img

  
suspendedComputeP :: (Unbox (PixelElt cs e), Unbox e) =>
                     Image RD cs e -> Image RD cs e
suspendedComputeP (RDImagePx arr) = RUImagePx . R.suspendedComputeP $ arr
suspendedComputeP (RDImageCh arr) = RUImageCh . R.suspendedComputeP $ arr
suspendedComputeP img             = img

{-
computeP :: (Unbox (PixelElt cs e), Unbox e, Monad m) =>
            Image RD cs e -> Image RD cs e
computeP (RDImagePx arr) = RUImagePx . R.computeP $ arr
computeP (RDImageCh arr) = RUImageCh . runIdentity . R.computeP $ arr
computeP img             = img
-}

computeS :: (Unbox (PixelElt cs e), Unbox e) =>
            Image RD cs e -> Image RD cs e
computeS (RDImagePx arr) = RUImagePx . R.computeS $ arr
computeS (RDImageCh arr) = RUImageCh . R.computeS $ arr
computeS img             = img



cond :: Bool -> t -> t -> t                                    
cond b t1 t2 = if b then t1 else t2




instance Elt RD cs e => Array RD cs e where
  type Elt RD cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e))

  data Image RD cs e where
    RUImageSc :: R.Array U DIM1 e               -> Image RD cs e
    RUImageCh :: R.Array U DIM3 e               -> Image RD cs e
    RUImagePx :: R.Array U DIM2 (PixelElt cs e) -> Image RD cs e
    RDImageCh :: R.Array D DIM3 e               -> Image RD cs e
    RDImagePx :: R.Array D DIM2 (PixelElt cs e) -> Image RD cs e

  dims (RUImageSc _                             ) = (1, 1)
  dims (RUImageCh (extent -> (Z :. _ :. m :. n))) = (m, n)
  dims (RUImagePx (extent -> (     Z :. m :. n))) = (m, n)
  dims (RDImageCh (extent -> (Z :. _ :. m :. n))) = (m, n)
  dims (RDImagePx (extent -> (     Z :. m :. n))) = (m, n)

  index (RUImageSc arr) c _      = R.index arr (Z :. c)
  index (RUImageCh arr) c (i, j) = R.index arr (Z :. c :. i :. j) 
  index (RUImagePx arr) c (i, j) =
    getEltCh (R.index arr (Z :. i :. j)) (undefined :: cs) c
  index (RDImageCh arr) c (i, j) = R.index arr (Z :. c :. i :. j) 
  index (RDImagePx arr) c (i, j) =
    getEltCh (R.index arr (Z :. i :. j)) (undefined :: cs) c

  indexPx img              ix     = fromElt $ indexElt img ix

  indexElt (RDImagePx arr) (i, j) = R.index arr (Z :. i :. j)
  indexElt (RUImagePx arr) (i, j) = R.index arr (Z :. i :. j)
  indexElt img             ix     = indexElt' img (undefined :: cs) ix

  singleton px = RUImageSc . R.computeS $ fromFunction (Z :. pixelRank px) getCh' where
    getCh' (Z :. c) = getPxCh px c

  make (m, n) f = RDImagePx $ fromFunction (Z :. m :. n) getPx' where
    getPx' (Z :. i :. j) = toElt $ f (i, j)

  map f (RUImageSc arr) = RUImageSc . R.computeS $ R.map f arr
  map f (RDImageCh arr) = RDImageCh $ R.map f arr
  map f img             = map f (disperse img)

  mapElt f (RDImagePx arr) = RDImagePx $ R.map f arr
  mapElt f img             = mapElt f (group img)

  mapPx f = mapElt (toElt . f . fromElt) 

  imap f (RDImageCh arr :: Image RD cs e) = RDImageCh $ R.traverse arr id f'
    where f' getCh' sh@(Z :. c :. i :. j) = f (i, j) c (getCh' sh)
  imap f img = imap f (disperse img)

  zipWith f (RUImageSc arr1)   (RUImageSc arr2)   = RUImageSc . R.computeS $ R.zipWith f arr1 arr2
  zipWith f (RDImageCh arr1)   (RDImageCh arr2)   = RDImageCh $ R.zipWith f arr1 arr2
  zipWith f (RUImageSc arr1)   (RDImageCh arr2)   = RDImageCh $ R.traverse arr2 id f'
    where f' getCh' sh@(Z :. c  :. _ :. _) = f (R.index arr1 (Z :. c)) (R.index arr2 sh)
  zipWith f img1@(RDImageCh _) img2@(RUImageSc _) = zipWith (flip f) img2 img1
  zipWith f img1               img2               = zipWith f (disperse img1) (disperse img2)

  zipWithElt f img1@(RUImageSc _) img2@(RUImageSc _) =
    singleton $ fromElt $ f (indexElt img1 (0, 0)) (indexElt img2 (0, 0))
  zipWithElt f img1@(RUImageSc _) img2@(RDImagePx _) = mapElt (f (indexElt img1 (0, 0))) img2
  zipWithElt f img1@(RDImagePx _) img2@(RUImageSc _) = mapElt (`f` (indexElt img2 (0, 0))) img1
  zipWithElt f (RDImagePx arr1)   (RDImagePx arr2)   = RDImagePx $ R.zipWith f arr1 arr2
  zipWithElt f img1               img2               = zipWithElt f (group img1) (group img2)
  
  zipWithPx f = zipWithElt func where
    func elt1 elt2 = toElt $ f (fromElt elt1) (fromElt elt2)

  traverse (RUImageSc _)     _       _     = error _error_traverse_scalar
  traverse (RDImageCh arr)   newDims newCh = RDImageCh $ R.traverse arr newDims' newCh' where
    newDims' (Z :. c :. m :. n) = let (m', n') = newDims (m, n) in (Z :. c :. m' :. n')
    newCh' getCh' (Z :. c :. i :. j) =
      let getCh'' (i', j') = getCh' (Z :. c :. i' :. j') in newCh getCh'' (i, j)
  traverse img               newDims newCh = traverse (disperse img) newDims newCh

  traverseCh (RUImageSc _)     _       _     = error _error_traverse_scalar
  traverseCh (RDImageCh arr)   newDims newCh = RDImageCh $ R.traverse arr newDims' newCh' where
    newDims' (Z :. c :. m :. n) = uncurry (ix3 c) $ newDims (m, n)
    newCh' getCh' (Z :. c :. i :. j) =
      let getCh'' c' (i', j') = getCh' (Z :. c' :. i' :. j') in newCh getCh'' c (i, j)
  traverseCh img               newDims newCh = traverseCh (disperse img) newDims newCh

  traversePx (RUImageSc _)     _       _     = error _error_traverse_scalar
  traversePx (RDImagePx arr)   newDims newPx = RDImagePx $ R.traverse arr newDims' newElt where
    newDims' (Z :. m :. n) = uncurry ix2 $ newDims (m, n)
    newElt getElt (Z :. i :. j) = let getPx (i', j') = fromElt $ getElt (Z :. i' :. j')
                                  in toElt $ newPx getPx (i, j)        
  traversePx img               newDims newPx = traversePx (group img) newDims newPx
    
  group img@(RUImageCh _)                              = group . delayImage $ img
  group img@(RDImageCh (extent -> (Z :. _ :. m :. n))) =
    RDImagePx $ fromFunction (Z :. m :. n) getElt' where
      getElt' (Z :. i :. j) = indexElt' img (undefined :: cs) (i, j)
  group img = delayImage img

  disperse img@(RUImagePx _) = disperse . delayImage $ img
  disperse (RDImagePx arr)   = RDImageCh $ R.traverse arr getSh' getCh' where
    getSh' (Z :. m :. n) = Z :. (rank (undefined :: cs)) :. m :. n
    getCh' getPx' (Z :. c :. i :. j) =
      getEltCh (getPx' (Z :. i :. j)) (undefined :: cs) c
  disperse img = delayImage img


instance Elt RS cs e => Array RS cs e where
  type Elt RS cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e))
  
  data Image RS cs e where
    RSImage :: Image RD cs e -> Image RS cs e

  dims (RSImage img) = dims img

  index (RSImage img) = index img

  indexElt (RSImage img) = indexElt img

  indexPx (RSImage img) = indexPx img

  make ix = RSImage . computeS . make ix

  singleton = RSImage . singleton

  map f (RSImage img) = RSImage . computeS . map f $ img

  mapElt f (RSImage img) = RSImage . computeS . mapElt f $ img
  
  mapPx f (RSImage img) = RSImage . computeS . mapPx f $ img

  imap f (RSImage img) = RSImage . computeS . imap f $ img

  zipWith f (RSImage img1) (RSImage img2) =
    RSImage . computeS . zipWith f img1 $ img2

  zipWithElt f (RSImage img1) (RSImage img2) =
    RSImage . computeS . zipWithElt f img1 $ img2

  zipWithPx f (RSImage img1) (RSImage img2) =
    RSImage . computeS . zipWithPx f img1 $ img2

  traverse (RSImage img) newDims newCh =
    RSImage . computeS . traverse img newDims $ newCh

  traverseCh (RSImage img) newDims newCh =
    RSImage . computeS . traverseCh img newDims $ newCh
    
  traversePx (RSImage img) newDims newCh =
    RSImage . computeS . traversePx img newDims $ newCh
    
  disperse (RSImage img) = RSImage . computeS . disperse $ img

  group (RSImage img) = RSImage . computeS . group $ img



instance Elt RP cs e => Array RP cs e where
  type Elt RP cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e))
  
  data Image RP cs e where
    RPImage :: Image RD cs e -> Image RP cs e

  dims (RPImage img) = dims img

  index (RPImage img) = index img

  indexElt (RPImage img) = indexElt img

  indexPx (RPImage img) = indexPx img

  make ix = RPImage . suspendedComputeP . make ix

  singleton = RPImage . singleton

  map f (RPImage img) = RPImage . suspendedComputeP . map f $ img

  mapElt f (RPImage img) = RPImage . suspendedComputeP . mapElt f $ img
  
  mapPx f (RPImage img) = RPImage . suspendedComputeP . mapPx f $ img

  imap f (RPImage img) = RPImage . suspendedComputeP . imap f $ img

  zipWith f (RPImage img1) (RPImage img2) =
    RPImage . suspendedComputeP . zipWith f img1 $ img2

  zipWithElt f (RPImage img1) (RPImage img2) =
    RPImage . suspendedComputeP . zipWithElt f img1 $ img2

  zipWithPx f (RPImage img1) (RPImage img2) =
    RPImage . suspendedComputeP . zipWithPx f img1 $ img2

  traverse (RPImage img) newDims newCh =
    RPImage . suspendedComputeP . traverse img newDims $ newCh

  traverseCh (RPImage img) newDims newCh =
    RPImage . suspendedComputeP . traverseCh img newDims $ newCh
    
  traversePx (RPImage img) newDims newCh =
    RPImage . suspendedComputeP . traversePx img newDims $ newCh
    
  disperse (RPImage img) = RPImage . suspendedComputeP . disperse $ img

  group (RPImage img) = RPImage . suspendedComputeP . group $ img



_error_traverse_scalar :: String
_error_traverse_scalar =
  "Traversal of a scalar image does not make sense, hence it is not implemented."
