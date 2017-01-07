{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Sparse (
  VS(..), Image(..), fromUnboxedVector, toUnboxedVector, fromIx, toIx, checkDims
  ) where

import Prelude as P

import Control.DeepSeq (deepseq)
-- import Control.Monad (void)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor
#endif
import Data.Typeable (Typeable)
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
--import qualified Data.Vector.Unboxed.Mutable as MV
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector.Unboxed
import Graphics.Image.Interface.Vector.Unboxing()


-- | Sparse unboxed matrix representation.
data VS = VS


instance Show VS where
  show _ = "VectorSparse"


instance Elt VS cs e => BaseArray VS cs e where
  type Elt VS cs e = (ColorSpace cs, Num e, Unbox e, Typeable e, 
                      Unbox (PixelElt cs e), Unbox (Pixel cs e), Eq (Pixel cs e))

  data Image VS cs e = SScalar (Pixel cs e)
                     | VSImage !Int !Int !(Vector ((Int, Int), Pixel cs e))

  dims (SScalar _)     = (1, 1)
  dims (VSImage m n _) = (m, n)
  {-# INLINE dims #-}



joinWith
  :: (Unbox px, Unbox a, Ord a)
  => (px -> px -> px)
  -> Vector (a, px)
  -> Vector (a, px)
  -> Vector (a, px)
joinWith f v1 v2 = V.unfoldr join (0, 0)
  where
    v1Len = V.length v1
    v2Len = V.length v2
    join (v1Ix, v2Ix)
      | v1Ix == v1Len && v2Ix == v2Len = Nothing
      | v1Ix == v1Len = Just $ (vunsafeIndex v2 v2Ix, (v1Ix, v2Ix + 1))
      | v2Ix == v2Len = Just $ (vunsafeIndex v1 v1Ix, (v1Ix + 1, v2Ix))
      | otherwise =
        let ((ix1, px1), (ix2, px2)) =
              (vunsafeIndex v1 v1Ix, vunsafeIndex v2 v2Ix)
        in case compare ix1 ix2 of
             EQ -> Just $ ((ix1, f px1 px2), (v1Ix + 1, v2Ix + 1))
             LT -> Just $ ((ix1, px1), (v1Ix + 1, v2Ix))
             GT -> Just $ ((ix2, px2), (v1Ix, v2Ix + 1))


joinWith2
  :: (Num px1, Unbox px1, Num px2, Unbox px2, Num px, Unbox px, Unbox a, Ord a)
  => (px1 -> px2 -> px)
  -> Vector (a, px1)
  -> Vector (a, px2)
  -> Vector (a, px)
joinWith2 f v1 v2 = V.unfoldr join (0, 0)
  where
    v1Len = V.length v1
    v2Len = V.length v2
    join (v1Ix, v2Ix)
      | v1Ix == v1Len && v2Ix == v2Len = Nothing
      | v1Ix == v1Len = let (ix, px) = vunsafeIndex v2 v2Ix in
                            Just $ ((ix, f 0 px), (v1Ix, v2Ix + 1))
      | v2Ix == v2Len = let (ix, px) = vunsafeIndex v1 v1Ix in
                        Just $ ((ix, f px 0), (v1Ix + 1, v2Ix))
      | otherwise =
        let ((ix1, px1), (ix2, px2)) =
              (vunsafeIndex v1 v1Ix, vunsafeIndex v2 v2Ix)
        in case compare ix1 ix2 of
             EQ -> Just $ ((ix1, f px1 px2), (v1Ix + 1, v2Ix + 1))
             LT -> Just $ ((ix1, f px1 0), (v1Ix + 1, v2Ix))
             GT -> Just $ ((ix2, f 0 px2), (v1Ix, v2Ix + 1))


vunsafeIndex :: Unbox a => Vector a -> Int -> a
vunsafeIndex = (V.!)


reconstructWith
  :: (Unbox b, Unbox a, Ord a)
  => Int -> Int -> Vector (a, b) -> ((Int, Int) -> (a, b)) -> Vector (a, b)
reconstructWith m n v f = joinWith const v $ V.generate (m*n) (f . toIx n) where


instance BaseArray VS cs e => Array VS cs e where

  type Manifest VS = VU

  makeImage (m, n) !f = VSImage m n $ filterZeros $ V.generate (m * n) mkPxIx
    where mkPxIx !k = let !ix = toIx n k in (ix, f ix)
  {-# INLINE makeImage #-}

  singleton = SScalar
  {-# INLINE singleton #-}

  index00 (SScalar px) = px
  index00 (VSImage _ _ v) = maybe 0 snd $ V.find ((== (0,0)) . fst) v
  {-# INLINE index00 #-}

  map f (SScalar px)    = SScalar (f px)
  map f (VSImage m n v) = VSImage m n $ filterZeros (getNewVec (f 0))
    where getNewVec 0  = V.map (fmap f) v
          getNewVec px = reconstructWith m n (V.map (fmap f) v) (,px)
          {-# INLINE getNewVec #-}
  {-# INLINE map #-}

  imap f (SScalar px)    = SScalar (f (0,0) px)
  imap f (VSImage m n v) = VSImage m n $ filterZeros newVec
    where f' !ix !px = (ix, f ix px)
          !newVec = reconstructWith m n (V.map (uncurry f') v) (`f'` 0)
  {-# INLINE imap #-}

  zipWith f (SScalar px1) (SScalar px2) = SScalar (f px1 px2)
  zipWith f (SScalar px1) !img2 = I.map (f px1) img2
  zipWith f !img1 (SScalar px2) = I.map (`f` px2) img1
  zipWith f img1@(VSImage m1 n1 v1) img2@(VSImage m2 n2 v2) =
    if m1 /= m2 || n1 /= n2
    then error ("zipWith: Images must be of the same dimensions, received: "++
                show img1++" and "++show img2++".")
    else VSImage m1 n1 $ filterZeros $
         case f 0 0 of
           0 -> joinWith2 f v1 v2
           px -> reconstructWith m1 n1 (joinWith2 f v1 v2) (,px)
  {-# INLINE zipWith #-}

  -- izipWith !f (VScalar px1) (VScalar px2)    = VScalar (f (0, 0) px1 px2)
  -- izipWith !f (VScalar px1) (VUImage m n v2) =
  --   VUImage m n (V.imap (\ !k !px2 -> f (toIx n k) px1 px2) v2)
  -- izipWith !f (VUImage m n v1) (VScalar px2) =
  --   VUImage m n (V.imap (\ !k !px1 -> f (toIx n k) px1 px2) v1)
  -- izipWith !f img1@(VUImage m1 n1 v1) img2@(VUImage m2 n2 v2) =
  --   if m1 /= m2 || n1 /= n2
  --   then error ("izipWith: Images must be of the same dimensions, received: "++
  --               show img1++" and "++show img2++".")
  --   else VUImage m1 n1 (V.izipWith (\ !k !px1 !px2 -> f (toIx n1 k) px1 px2) v1 v2)
  -- {-# INLINE izipWith #-}

  -- traverse !img !getNewDims !getNewPx = makeImage (getNewDims (dims img)) (getNewPx (index img))
  -- {-# INLINE traverse #-}

  -- traverse2 !img1 !img2 !getNewDims !getNewPx =
  --   makeImage (getNewDims (dims img1) (dims img2)) (getNewPx (index img1) (index img2))
  -- {-# INLINE traverse2 #-}

  -- transpose !img@(dims -> (m, n)) = makeImage (n, m) getPx where
  --   getPx !(i, j) = index img (j, i)
  --   {-# INLINE getPx #-}
  -- {-# INLINE transpose #-}

  -- backpermute !(checkDims "VU.backpermute" -> (m, n)) !f (VUImage _ n' v) =
  --   VUImage m n $ V.backpermute v $ V.generate (m*n) (fromIx n' . f . toIx n)
  -- backpermute !sz _ (VScalar px) = makeImage sz (const px)
  -- {-# INLINE backpermute #-}
  
  fromLists !ls = if all (== n) (P.map length ls)
                  then VSImage m n . V.map toIx' . V.filter ((/=0) . snd) .
                       V.indexed . V.concat . P.map V.fromList $ ls
                  else error "fromLists: Inner lists are of different lengths."
    where
      toIx' !(k, px) = (toIx n k, px)
      {-# INLINE toIx' #-}
      !(m, n) = checkDims "VU.fromLists" (length ls, length $ head ls)
  {-# INLINE fromLists #-}

  foldIx f !px0 (SScalar px) = f px0 (0,0) px
  foldIx f !px0 (VSImage _ _ v) = V.foldl' g px0 v where
    g !acc !(ix, px) = f acc ix px
  {-# INLINE foldIx #-}

  -- (|*|) img1@(VUImage m1 n1 v1) !img2@VUImage {} =
  --   if n1 /= m2 
  --   then error ("Inner dimensions of multiplying images must be the same, but received: "++
  --               show img1 ++" X "++ show img2)
  --   else
  --     makeImage (m1, n2) getPx where
  --       VUImage n2 m2 v2 = transpose img2
  --       getPx !(i, j) = V.sum $ V.zipWith (*) (V.slice (i*n1) n1 v1) (V.slice (j*m2) m2 v2)
  --       {-# INLINE getPx #-}
  -- (|*|) (VScalar px1) (VScalar px2) = VScalar (px1 * px2)
  -- (|*|) _ _ = error "Scalar Images cannot be multiplied."
  -- {-# INLINE (|*|) #-}

  -- eq (VUImage m1 n1 v1) (VUImage m2 n2 v2) =
  --   m1 == m2 && n1 == n2 && V.all id (V.zipWith (==) v1 v2)
  -- eq (VScalar px1)           (VScalar px2) = px1 == px2
  -- eq (VUImage 1 1 v1) (VScalar px2) = v1 V.! 0 == px2
  -- eq (VScalar px1) (VUImage 1 1 v2) = v2 V.! 0 == px1
  -- eq _ _ = False
  -- {-# INLINE eq #-}

  compute (VSImage m n v) = m `seq` n `seq` v `deepseq` (VSImage m n v)
  compute (SScalar px)    = px `seq` SScalar px
  {-# INLINE compute #-}

  toManifest (SScalar px) = VScalar px
  toManifest (VSImage m n v) = VUImage m n $ V.map snd $ reconstructWith m n v (,0)
  {-# INLINE toManifest #-}

-- data W arr = W arr

-- instance Show arr => Show (W arr) where
--   show (W arr) = "W (" ++ show arr ++ ")"




-- instance BaseArray arr cs e => BaseArray (W arr) cs e where
--   type Elt (W arr) cs e = Elt arr cs e

--   data Image (W arr) cs e = WImage { wRows   :: !Int
--                                    , wCols   :: !Int
--                                    , wWindow :: !(Int, Int, Int, Int)
--                                    , wImg    :: !(Image arr cs e) }

--   dims (WImage m n _ _) = (m, n)
--   {-# INLINE dims #-}


-- instance MArray arr cs e => MArray (W arr) cs e where
  
--    data MImage st (W arr) cs e = MWImage { mwRows   :: !Int
--                                          , mwCols   :: !Int
--                                          , mwWindow :: !(Int, Int, Int, Int)
--                                          , mwImg    :: !(MImage st arr cs e) }


--    --index :: Image arr cs e -> (Int, Int) -> Pixel cs e
--    --index (WImage (m, n) (it, jt, ib, jb) img) (i, j) =

--    unsafeIndex (WImage _ _ _ img) = unsafeIndex img
--    {-# INLINE unsafeIndex #-}


filterZeros :: (Num px, Eq px, Unbox px, Unbox a) => Vector (a, px) -> Vector (a, px)
filterZeros = V.filter ((/=0) . snd)
{-# INLINE filterZeros #-}


instance Exchangable VU VS where
  exchange _ (VScalar px) = SScalar px
  exchange _ (VUImage m n v) = VSImage m n $ filterZeros $ V.imap addIx v where
    addIx k px = (toIx n k, px)


instance Exchangable VS VU where
  exchange _ = toManifest
