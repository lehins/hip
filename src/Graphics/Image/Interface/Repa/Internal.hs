{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Interface.Repa.Internal
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa.Internal (
  RP(..), RS(..),
  fromRepaArrayS, fromRepaArrayP, toRepaArray
  ) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (map, zipWith, foldl, foldr, mapM, mapM_, read, traverse)
#else
import Prelude hiding (map, zipWith, foldl, foldr, mapM, mapM_, read)
#endif
import qualified Prelude as P (map, mapM_)
import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Binary (Bit(..))
import Graphics.Image.Interface.Vector.Unboxed
       (VU(..), fromUnboxedVector, toUnboxedVector, checkDims)
import Graphics.Image.Interface.Vector.Sparse (VS(..))
import Graphics.Image.Interface.Repa.Helpers


import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V (singleton)

import Data.Typeable (Typeable)
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R (Elt(..), suspendedComputeP)


-- | Repa 'U'nboxed Array representation, which is computed in parallel.
data RP = RP

-- | Repa 'U'nboxed Array representation, which is computed sequentially. 
data RS = RS

instance Show RP where
  show _ = "RepaParallel"
  
instance Show RS where
  show _ = "RepaSequential"



instance Elt RS cs e => BaseArray RS cs e where
  type Elt RS cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e, Typeable e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RS cs e = SScalar !(Pixel cs e)
                     | SUImage !(R.Array R.U R.DIM2 (Pixel cs e))
                     | SDImage !(R.Array R.D R.DIM2 (Pixel cs e))
                       
  dims (SScalar _                          ) = (1, 1)
  dims (SUImage (R.extent -> (Z :. m :. n))) = (m, n)
  dims (SDImage (R.extent -> (Z :. m :. n))) = (m, n)
  {-# INLINE dims #-}


instance (BaseArray RS cs e) => Array RS cs e where

  type Manifest RS = VU
  
  makeImage !(checkDims "RS.makeImage" -> (m, n)) !f =
    SDImage $ R.fromFunction (Z :. m :. n) (f . sh2dims)
  {-# INLINE makeImage #-}
  
  singleton = SScalar
  {-# INLINE singleton #-}

  index00 (SScalar px)  = px
  index00 (SUImage arr) = R.index arr (Z :. 0 :. 0)
  index00 (SDImage arr) = R.index arr (Z :. 0 :. 0)
  {-# INLINE index00 #-}

  map !f (SScalar px)  = SScalar (f px)
  map !f (SUImage arr) = SDImage (R.map f arr)
  map !f (SDImage arr) = SDImage (R.map f arr)
  {-# INLINE map #-}

  imap !f (SScalar px)  = SScalar (f (0, 0) px)
  imap !f (SUImage arr) = SDImage (imapR f arr)
  imap !f (SDImage arr) = SDImage (imapR f arr)
  {-# INLINE imap #-}

  zipWith f (SScalar px1) (SScalar px2) = SScalar (f px1 px2)
  zipWith f (SScalar px1) !img2         = map (f px1) img2
  zipWith f !img1         (SScalar px2) = map (`f` px2) img1
  zipWith f !img1         !img2         =
    SDImage (R.zipWith f (getDelayedS img1) (getDelayedS img2))
  {-# INLINE zipWith #-}

  izipWith f (SScalar px1) (SScalar px2) = SScalar (f (0, 0) px1 px2)
  izipWith f (SScalar px1) !img2         = imap (`f` px1) img2
  izipWith f !img1         (SScalar px2) = imap (\ !ix !px -> f ix px px2) img1
  izipWith f !img1         !img2         =
    SDImage (izipWithR f (getDelayedS img1) (getDelayedS img2))
  {-# INLINE izipWith #-}
  
  traverse !img getNewDims getNewPx =
    SDImage (traverseR (getDelayedS img) getNewDims getNewPx)
  {-# INLINE traverse #-}

  traverse2 !img1 !img2 getNewDims getNewPx =
    SDImage (traverse2R (getDelayedS img1) (getDelayedS img2) getNewDims getNewPx)
  {-# INLINE traverse2 #-}

  transpose (SDImage arr) = SDImage (R.transpose arr)
  transpose (SUImage arr) = SDImage (R.transpose arr)
  transpose !img          = img
  {-# INLINE transpose #-}

  backpermute !newDims g !img = SDImage (backpermuteR (getDelayedS img) newDims g)
  {-# INLINE backpermute #-}

  fromLists = SUImage . fromListsR
  {-# INLINE fromLists #-}

  fold f !px0 (SDImage arr) = R.foldAllS f px0 arr
  fold f !px0 (SUImage arr) = R.foldAllS f px0 arr
  fold f !px0 (SScalar px)  = f px px0
  {-# INLINE fold #-}

  foldIx f !px0 (SDImage arr) = foldIxS f px0 arr
  foldIx f !px0 (SUImage arr) = foldIxS f px0 arr
  foldIx f !px0 (SScalar px)  = f px0 (0, 0) px
  {-# INLINE foldIx #-}


  eq (SScalar px1) (SScalar px2) = px1 == px2
  eq !img1 !img2 = R.equalsS (getDelayedS img1) (getDelayedS img2)
  {-# INLINE eq #-}

  compute !img@(SScalar _) = img
  compute !img@(SUImage _) = img
  compute (SDImage arr)   = SUImage $ R.computeS arr
  {-# INLINE compute #-}

  (|*|) (SUImage arr1)   (SUImage arr2)   = SDImage (multR arr1 arr2)
  (|*|) img1@(SDImage _) !img2            = compute img1 |*| img2
  (|*|) !img1            img2@(SDImage _) = img1 |*| compute img2
  (|*|) (SScalar px1)    !img2            = SUImage (singletonR px1) |*| img2
  (|*|) !img1            (SScalar px2)    = img1 |*| SUImage (singletonR px2)
  {-# INLINE (|*|) #-}

  toManifest img@(SUImage arr) = fromUnboxedVector (dims img) (R.toUnboxed arr)
  toManifest (SScalar px)      = singleton px
  toManifest !img              = toManifest (compute img)
  {-# INLINE toManifest #-}

---------------------
-- Parallel Arrays --
---------------------

instance Elt RP cs e => BaseArray RP cs e where
  type Elt RP cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e, Typeable e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RP cs e = PScalar !(Pixel cs e)
                     | PUImage !(R.Array R.U R.DIM2 (Pixel cs e))
                     | PDImage !(R.Array R.D R.DIM2 (Pixel cs e))
                       
  dims (PScalar _                          ) = (1, 1)
  dims (PUImage (R.extent -> (Z :. m :. n))) = (m, n)
  dims (PDImage (R.extent -> (Z :. m :. n))) = (m, n)
  {-# INLINE dims #-}


instance (BaseArray RP cs e) => Array RP cs e where

  type Manifest RP = VU
  
  makeImage !(checkDims "RP.makeImage" -> (m, n)) !f =
    PDImage $ R.fromFunction (Z :. m :. n) (f . sh2dims)
  {-# INLINE makeImage #-}

  makeImageWindowed !(checkDims "RP.makeImage" -> (m, n)) !window getWindowPx getBorderPx  =
    PDImage $ R.delay $ makeWindowed (Z :. m :. n) window
    (R.fromFunction (Z :. m :. n) (getWindowPx . sh2dims))
    --(hintSmall
     (R.fromFunction (Z :. m :. n) (getBorderPx . sh2dims))
    
  {-# INLINE makeImageWindowed #-}
  
  singleton = PScalar
  {-# INLINE singleton #-}

  index00 (PScalar px)  = px
  index00 (PUImage arr) = R.index arr (Z :. 0 :. 0)
  index00 (PDImage arr) = R.index arr (Z :. 0 :. 0)
  {-# INLINE index00 #-}

  map f (PScalar px)  = PScalar (f px)
  map f (PUImage arr) = PDImage (R.map f arr)
  map f (PDImage arr) = PDImage (R.map f arr)
  {-# INLINE map #-}

  imap f (PScalar px)  = PScalar (f (0, 0) px)
  imap f (PUImage arr) = PDImage (imapR f arr)
  imap f (PDImage arr) = PDImage (imapR f arr)
  {-# INLINE imap #-}

  zipWith f (PScalar px1)  (PScalar px2) = PScalar (f px1 px2)
  zipWith f (PScalar px1)  !img2         = map (f px1) img2
  zipWith f !img1          (PScalar px2) = map (`f` px2) img1
  zipWith f !img1          !img2         =
    PDImage (R.zipWith f (getDelayedP img1) (getDelayedP img2))
  {-# INLINE zipWith #-}

  izipWith f (PScalar px1)  (PScalar px2) = PScalar (f (0, 0) px1 px2)
  izipWith f (PScalar px1)  !img2         = imap (`f` px1) img2
  izipWith f !img1          (PScalar px2) = imap (\ !ix !px -> f ix px px2) img1
  izipWith f !img1          !img2         =
    PDImage (izipWithR f (getDelayedP img1) (getDelayedP img2))
  {-# INLINE izipWith #-}
  
  traverse !img          getNewDims getNewPx =
    PDImage (traverseR (getDelayedP img) getNewDims getNewPx)
  {-# INLINE traverse #-}

  traverse2 !img1 !img2 getNewDims getNewPx =
    PDImage (traverse2R (getDelayedP img1) (getDelayedP img2) getNewDims getNewPx)
  {-# INLINE traverse2 #-}

  transpose (PDImage arr) = PDImage (R.transpose arr)
  transpose (PUImage arr) = PDImage (R.transpose arr)
  transpose !img          = img
  {-# INLINE transpose #-}

  backpermute !newDims g !img = PDImage (backpermuteR (getDelayedP img) newDims g)
  {-# INLINE backpermute #-}

  fromLists = PUImage . fromListsR
  {-# INLINE fromLists #-}

  fold f !px0 (PDImage arr) = head $ R.foldAllP f px0 arr
  fold f !px0 (PUImage arr) = head $ R.foldAllP f px0 arr
  fold f !px0 (PScalar px)  = f px px0
  {-# INLINE fold #-}

  foldIx f !px0 (PDImage arr) = head $ foldIxP f px0 arr
  foldIx f !px0 (PUImage arr) = head $ foldIxP f px0 arr
  foldIx f !px0 (PScalar px)  = f px0 (0, 0) px
  {-# INLINE foldIx #-}
  
  eq (PScalar px1) (PScalar px2) = px1 == px2
  eq !img1 !img2 = R.equalsS (getDelayedP img1) (getDelayedP img2)
  {-# INLINE eq #-}

  compute img@(PScalar _) = img
  compute img@(PUImage _) = img
  compute (PDImage arr)   = arrU `R.deepSeqArray` PUImage arrU
    where arrU = R.suspendedComputeP arr
  {-# INLINE compute #-}

  (|*|) (PUImage arr1)   (PUImage arr2)   = PDImage (multR arr1 arr2)
  (|*|) img1@(PDImage _) !img2            = compute img1 |*| img2
  (|*|) !img1            img2@(PDImage _) = img1 |*| compute img2
  (|*|) (PScalar px1)    !img2            = PUImage (singletonR px1) |*| img2
  (|*|) !img1            !(PScalar px2)   = img1 |*| PUImage (singletonR px2)
  {-# INLINE (|*|) #-}

  toManifest img@(PUImage arr) = fromUnboxedVector (dims img) (R.toUnboxed arr)
  toManifest (PScalar px)      = singleton px
  toManifest !img              = toManifest (compute img)
  {-# INLINE toManifest #-}


----------------------
-- Helper functions --
----------------------

sh2dims :: DIM2 -> (Int, Int)
sh2dims (Z :. i :. j) = (i, j)
{-# INLINE sh2dims #-}

dims2sh :: (Int, Int) -> DIM2
dims2sh !(i, j) = Z :. i :. j 
{-# INLINE dims2sh #-}


imapR
  :: R.Source r2 b =>
     ((Int, Int) -> b -> c) -> R.Array r2 DIM2 b -> R.Array R.D DIM2 c
imapR f !arr = R.zipWith f (R.fromFunction (R.extent arr) sh2dims) arr


-- | Combine two arrays, element-wise, with index aware operator. If the extent of
-- the two array arguments differ, then the resulting array's extent is their
-- intersection.
izipWithR
  :: (R.Source r2 t1, R.Source r1 t)
  => ((Int, Int) -> t -> t1 -> c)
  -> R.Array r1 DIM2 t
  -> R.Array r2 DIM2 t1
  -> R.Array R.D DIM2 c
izipWithR f !arr1 !arr2 =
  (R.traverse2 arr1 arr2 getNewDims getNewPx) where
    getNewPx !getPx1 !getPx2 !sh = f (sh2dims sh) (getPx1 sh) (getPx2 sh)
    getNewDims (Z :. m1 :. n1) (Z :. m2 :. n2) = Z :. min m1 m2 :. min n1 n2
    {-# INLINE getNewPx #-}
{-# INLINE izipWithR #-}


traverseR
  :: R.Source r c
  => R.Array r DIM2 c
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> c) -> (Int, Int) -> b)
  -> R.Array R.D DIM2 b
traverseR !arr getNewDims getNewPx =
  R.traverse arr (dims2sh . checkDims "traverseR" . getNewDims . sh2dims) getNewE
  where
    getNewE getPx = getNewPx (getPx . dims2sh) . sh2dims
    {-# INLINE getNewE #-}
{-# INLINE traverseR #-}

traverse2R
  :: (R.Source r2 c1, R.Source r1 c)
  => R.Array r1 DIM2 c
  -> R.Array r2 DIM2 c1
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> c) -> ((Int, Int) -> c1) -> (Int, Int) -> c2)
  -> R.Array R.D DIM2 c2
traverse2R !arr1 !arr2 getNewDims getNewPx =
  R.traverse2 arr1 arr2 getNewSh getNewE
  where getNewE getPx1 getPx2 = getNewPx (getPx1 . dims2sh) (getPx2 . dims2sh) . sh2dims
        {-# INLINE getNewE #-}
        getNewSh !sh1 !sh2 =
          dims2sh . checkDims "traverse2R" $ getNewDims (sh2dims sh1) (sh2dims sh2)
        {-# INLINE getNewSh #-}
{-# INLINE traverse2R #-}

backpermuteR
  :: R.Source r e
  => R.Array r DIM2 e
  -> (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> R.Array R.D DIM2 e
backpermuteR !arr newDims g =
  R.backpermute
    (dims2sh (checkDims "backpermuteR" newDims))
    (dims2sh . g . sh2dims)
    arr
{-# INLINE backpermuteR #-}


fromListsR :: Unbox a => [[a]] -> R.Array R.U DIM2 a
fromListsR ls =
  if all (== n) (P.map length ls)
    then R.fromListUnboxed (Z :. m :. n) . concat $ ls
    else error "fromListsR: Inner lists do not all have an equal length."
  where
    !(m, n) = checkDims "fromListsR" (length ls, length $ head ls)
{-# INLINE fromListsR #-}



multR
  :: (Num a, Unbox a, R.Elt a)
  => R.Array R.U DIM2 a -> R.Array R.U DIM2 a -> R.Array R.D DIM2 a
multR !arr1 !arr2 =
  if n1 /= m2
    then error $
         "Inner dimensions of multiplied images must be the same, but received: " ++ ""
         --show img1 ++ " X " ++ show img2
    else R.fromFunction (Z :. m1 :. n2) $ getPx
  where
    (Z :. m1 :. n1) = R.extent arr1
    (Z :. m2 :. n2) = R.extent arr2
    getPx (Z :. i :. j) =
      R.sumAllS
        (R.slice arr1 (R.Any :. (i :: Int) :. R.All) R.*^
         R.slice arr2 (R.Any :. (j :: Int)))
    {-# INLINE getPx #-}
{-# INLINE multR #-}


singletonR :: Unbox e => e -> R.Array R.U DIM2 e
singletonR !px = R.fromUnboxed (Z :. 1 :. 1) $ V.singleton px


getDelayedS :: Array RS cs e => Image RS cs e -> R.Array R.D DIM2 (Pixel cs e)
getDelayedS (SUImage arr) = R.delay arr
getDelayedS (SDImage arr) = arr
getDelayedS (SScalar px)  = R.fromFunction (Z :. 1 :. 1) (const px)
{-# INLINE getDelayedS #-}

getDelayedP :: Array RP cs e => Image RP cs e -> R.Array R.D DIM2 (Pixel cs e)
getDelayedP (PUImage arr) = R.delay arr
getDelayedP (PDImage arr) = arr
getDelayedP (PScalar px)  = R.fromFunction (Z :. 1 :. 1) (const px)
{-# INLINE getDelayedP #-}


-- | Changes computation strategy. Will casue all fused operations to be computed.
instance Exchangable RP RS where
  
  exchange _ (PScalar px)   = SScalar px
  exchange _ (PUImage arr)  = SUImage arr
  exchange r img@(PDImage _) = exchange r (compute img)
  {-# INLINE exchange #-}


-- | Changes computation strategy. Will casue all fused operations to be computed.
instance Exchangable RS RP where
  
  exchange _ (SScalar px)   = PScalar px
  exchange _ (SUImage arr)  = PUImage arr
  exchange r img@(SDImage _) = exchange r (compute img)
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable VU RS where
  exchange _ img@(dims -> (1, 1)) = singleton (img `index` (0, 0))
  exchange _ img = SUImage . R.fromUnboxed (dims2sh $ dims img) . toUnboxedVector $ img
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable VU RP where
  exchange _ img@(dims -> (1, 1)) = singleton (img `index` (0, 0))
  exchange _ img = PUImage . R.fromUnboxed (dims2sh $ dims img) . toUnboxedVector $ img
  {-# INLINE exchange #-}


-- | Changes to Vector representation.
instance Exchangable RS VU where
  exchange _ = toManifest
  {-# INLINE exchange #-}


-- | Changes to Vector representation.
instance Exchangable RP VU where
  exchange _ = toManifest
  {-# INLINE exchange #-}


-- | Changes to Sparse Vector representation.
instance Exchangable RS VS where
  exchange arr = exchange arr . toManifest
  {-# INLINE exchange #-}


-- | Changes to Sparse Vector representation.
instance Exchangable RP VS where
  exchange arr = exchange arr . toManifest
  {-# INLINE exchange #-}


-- | Changes from Sparse Vector representation.
instance Exchangable VS RS where
  exchange arr = exchange arr . toManifest
  {-# INLINE exchange #-}


-- | Changes from Sparse Vector representation.
instance Exchangable VS RP where
  exchange arr = exchange arr . toManifest
  {-# INLINE exchange #-}


-- | Create a sequential image from a 2D Repa delayed array.
fromRepaArrayS :: R.Array R.D DIM2 (Pixel cs e) -> Image RS cs e
fromRepaArrayS = SDImage


-- | Create a parallel image from a 2D Repa delayed array.
fromRepaArrayP :: R.Array R.D DIM2 (Pixel cs e) -> Image RP cs e
fromRepaArrayP = PDImage


-- | Retrieve an underlying Repa array from an image.
toRepaArray
  :: (Array arr cs e, Array RS cs e, Exchangable arr RS)
  => Image arr cs e -> R.Array R.U DIM2 (Pixel cs e)
toRepaArray img =
  case compute (exchange RS img) of
    SUImage arr -> arr
    SDImage arr -> R.computeS arr -- shouldn't occur, but for completeness
    SScalar px -> R.computeS $ R.fromFunction (Z :. 1 :. 1) $ const px

instance R.Elt Bit where
  touch (Bit w) = R.touch w
  {-# INLINE touch #-}
  
  zero     = 0
  {-# INLINE zero #-}
  
  one      = 1
  {-# INLINE one #-}


instance (ColorSpace cs, R.Elt e, Num e) => R.Elt (Pixel cs e) where
  touch !px = P.mapM_ (R.touch . getPxCh px) (enumFrom (toEnum 0)) 
  {-# INLINE touch #-}
  
  zero     = 0
  {-# INLINE zero #-}
  
  one      = 1
  {-# INLINE one #-}


addIxArr
  :: R.Source r2 b =>
     R.Array r2 DIM2 b -> R.Array R.D DIM2 ((Int, Int), b)
addIxArr !arr = R.zipWith (,) arrIx arr
  where
    !arrIx = R.fromFunction (R.extent arr) sh2dims


foldIxS
  :: R.Source r2 b =>
     (b -> (Int, Int) -> b -> b) -> b -> R.Array r2 DIM2 b -> b
foldIxS f acc arr = snd $ R.foldAllS g ((-1, 0), acc) arr'
  where
    !arr' = addIxArr arr
    g (accIx@(-1, _), acc') (ix, px) = (accIx, f acc' ix px)
    g (ix, px) (accIx@(-1, _), acc') = (accIx, f acc' ix px)
    g (acc1Ix, _) (acc2Ix, _) =
      error $ "Impossible happened. Received: " ++ show acc1Ix ++ " " ++ show acc2Ix


foldIxP
  :: (R.Source r2 b, Unbox b, Monad f) =>
     (b -> (Int, Int) -> b -> b) -> b -> R.Array r2 DIM2 b -> f b
foldIxP f acc arr = snd <$> R.foldAllP g ((-1, 0), acc) arr'
  where
    !arr' = addIxArr arr
    g (accIx@(-1, _), acc') (ix, px) = (accIx, f acc' ix px)
    g (ix, px) (accIx@(-1, _), acc') = (accIx, f acc' ix px)
    g (acc1Ix, _) (acc2Ix, _) =
      error $ "Impossible happened. Received: " ++ show acc1Ix ++ " " ++ show acc2Ix
