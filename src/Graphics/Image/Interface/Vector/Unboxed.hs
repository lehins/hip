{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
module Graphics.Image.Interface.Vector.Unboxed (
  VU(..), Image(..), fromUnboxedVector, toUnboxedVector, fromIx, toIx
  ) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P (map)
import Control.DeepSeq (deepseq)
#if MIN_VERSION_base(4,8,0)
import Control.Monad (void)
#else
import Data.Functor
#endif
import Data.Typeable (Typeable)
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Graphics.Image.Interface
import Graphics.Image.Interface.Vector.Unboxing()


-- | Unboxed 'Vector' representation.
data VU = VU


instance Show VU where
  show _ = "VectorUnboxed"


instance Elt VU cs e => Array VU cs e where
  type Elt VU cs e = (ColorSpace cs, Num e, Unbox e, Typeable e, 
                      Unbox (PixelElt cs e), Unbox (Pixel cs e))
                     
  data Image VU cs e = VScalar !(Pixel cs e)
                     | VUImage !Int !Int !(Vector (Pixel cs e))
  
  makeImage !(m, n) !f = VUImage m n $ V.generate (m * n) (f . toIx n)
  {-# INLINE makeImage #-}

  singleton = VScalar
  {-# INLINE singleton #-}
  
  dims (VUImage m n _) = (m, n)
  dims _               = (1, 1)
  {-# INLINE dims #-}
  
  map !f (VScalar px)    = VScalar (f px)
  map !f (VUImage m n v) = VUImage m n (V.map f v)
  {-# INLINE map #-}

  imap !f (VScalar px)    = VScalar (f (0, 0) px)
  imap !f (VUImage m n v) = VUImage m n (V.imap (\ !k !px -> f (toIx n k) px) v)
  {-# INLINE imap #-}
  
  zipWith !f (VScalar px1) (VScalar px2)    = VScalar (f px1 px2)
  zipWith !f (VScalar px1) (VUImage m n v2) = VUImage m n (V.map (f px1) v2)
  zipWith !f (VUImage m n v1) (VScalar px2) = VUImage m n (V.map (`f` px2) v1)
  zipWith !f img1@(VUImage m1 n1 v1) img2@(VUImage m2 n2 v2) =
    if m1 /= m2 || n1 /= n2
    then error ("zipWith: Images must be of the same dimensions, received: "++
                show img1++" and "++show img2++".")
    else VUImage m1 n1 (V.zipWith f v1 v2)
  {-# INLINE zipWith #-}

  izipWith !f (VScalar px1) (VScalar px2)    = VScalar (f (0, 0) px1 px2)
  izipWith !f (VScalar px1) (VUImage m n v2) =
    VUImage m n (V.imap (\ !k !px2 -> f (toIx n k) px1 px2) v2)
  izipWith !f (VUImage m n v1) (VScalar px2) =
    VUImage m n (V.imap (\ !k !px1 -> f (toIx n k) px1 px2) v1)
  izipWith !f img1@(VUImage m1 n1 v1) img2@(VUImage m2 n2 v2) =
    if m1 /= m2 || n1 /= n2
    then error ("izipWith: Images must be of the same dimensions, received: "++
                show img1++" and "++show img2++".")
    else VUImage m1 n1 (V.izipWith (\ !k !px1 !px2 -> f (toIx n1 k) px1 px2) v1 v2)
  {-# INLINE izipWith #-}

  traverse !img !getNewDims !getNewPx = makeImage (getNewDims $ dims img) (getNewPx (index img))
  {-# INLINE traverse #-}

  traverse2 !img1 !img2 !getNewDims !getNewPx =
    makeImage (getNewDims (dims img1) (dims img2)) (getNewPx (index img1) (index img2))
  {-# INLINE traverse2 #-}

  transpose !img@(dims -> (m, n)) = makeImage (n, m) getPx where
    getPx !(i, j) = index img (j, i)
    {-# INLINE getPx #-}
  {-# INLINE transpose #-}

  backpermute !(m, n) !f (VUImage _ n' v) =
    VUImage m n $ V.backpermute v $ V.generate (m*n) (fromIx n' . f . toIx n)
  backpermute !sz      _ (VScalar px)     =
    if sz == (1, 1) then VScalar px else makeImage sz (const px)
  {-# INLINE backpermute #-}
  
  fromLists !ls = if isSquare
                  then VUImage m n . V.fromList . concat $ ls
                  else error "fromLists: Inner lists are of different lengths."
    where
      !(m, n) = (length ls, length $ head ls)
      !isSquare = (n > 0) && all (==n) (P.map length ls)
  {-# INLINE fromLists #-}


instance Array VU cs e => ManifestArray VU cs e where

  index (VUImage _ n v) !ix = v V.! fromIx n ix
  index (VScalar px)      _ = px
  {-# INLINE index #-}

  deepSeqImage (VUImage m n v) = m `seq` n `seq` deepseq v
  deepSeqImage (VScalar px)    = seq px
  {-# INLINE deepSeqImage #-}
  
  fold !f !px0 (VUImage _ _ v) = V.foldl' f px0 v
  fold !f !px0 (VScalar px)    = f px0 px
  {-# INLINE fold #-}

  (|*|) img1@(VUImage m1 n1 v1) !img2@VUImage {} =
    if n1 /= m2 
    then error ("Inner dimensions of multiplying images must be the same, but received: "++
                show img1 ++" X "++ show img2)
    else
      makeImage (m1, n2) getPx where
        VUImage n2 m2 v2 = transpose img2
        getPx !(i, j) = V.sum $ V.zipWith (*) (V.slice (i*n1) n1 v1) (V.slice (j*m2) m2 v2)
        {-# INLINE getPx #-}
  (|*|) (VScalar px1) (VScalar px2) = VScalar (px1 * px2)
  (|*|) _ _ = error "Scalar Images cannot be multiplied."
  {-# INLINE (|*|) #-}

  eq (VUImage m1 n1 v1) (VUImage m2 n2 v2) =
    m1 == m2 && n1 == n2 && V.all id (V.zipWith (==) v1 v2)
  eq (VScalar px1)           (VScalar px2) = px1 == px2
  eq _                       _             = False
  {-# INLINE eq #-}


instance ManifestArray VU cs e => SequentialArray VU cs e where

  foldl !f !a (VUImage _ _ v) = V.foldl' f a v
  foldl !f !a (VScalar px)    = f a px
  {-# INLINE foldl #-}

  foldr !f !a (VUImage _ _ v) = V.foldr' f a v
  foldr !f !a (VScalar px)    = f px a
  {-# INLINE foldr #-}

  mapM !f (VUImage m n v) = VUImage m n <$> V.mapM f v
  mapM !f (VScalar px)    = VScalar <$> f px
  {-# INLINE mapM #-}

  mapM_ !f (VUImage _ _ v) = V.mapM_ f v
  mapM_ !f (VScalar px)    = void $ f px
  {-# INLINE mapM_ #-}

  foldM !f !a (VUImage _ _ v) = V.foldM' f a v
  foldM !f !a (VScalar px)    = f a px
  {-# INLINE foldM #-}

  foldM_ !f !a (VUImage _ _ v) = V.foldM'_ f a v
  foldM_ !f !a (VScalar px)    = void $ f a px
  {-# INLINE foldM_ #-}


instance ManifestArray VU cs e => MutableArray VU cs e where

  data MImage st VU cs e = MVImage !Int !Int (MV.MVector st (Pixel cs e))
                         | MVScalar (MV.MVector st (Pixel cs e))

  mdims (MVImage m n _) = (m, n)
  mdims (MVScalar _)    = (1, 1)
  {-# INLINE mdims #-}

  thaw (VUImage m n v) = MVImage m n <$> V.thaw v
  thaw (VScalar px)    = MVScalar <$> V.thaw (V.singleton px)
  {-# INLINE thaw #-}

  freeze (MVImage m n mv) = VUImage m n <$> V.freeze mv
  freeze (MVScalar mv)    = VScalar . (V.! 0) <$> V.freeze mv
  {-# INLINE freeze #-}

  new (m, n) = MVImage m n <$> MV.new (m*n)
  {-# INLINE new #-}

  read (MVImage _ n mv) ix = MV.read mv (fromIx n ix)
  read (MVScalar mv)    _  = MV.read mv 0
  {-# INLINE read #-}

  write (MVImage _ n mv) ix = MV.write mv (fromIx n ix)
  write (MVScalar mv)    _  = MV.write mv 0
  {-# INLINE write #-}

  swap (MVImage _ n mv) ix1 ix2 = MV.swap mv (fromIx n ix1) (fromIx n ix2)
  swap _                _   _   = return ()
  {-# INLINE swap #-}


-- | Convert an image to a flattened Unboxed 'Vector'. It is a __O(1)__ opeartion.
--
-- >>> toUnboxedVector $ makeImage (3, 2) (\(i, j) -> PixelY $ fromIntegral (i+j))
-- fromList [<Luma:(0.0)>,<Luma:(1.0)>,<Luma:(1.0)>,<Luma:(2.0)>,<Luma:(2.0)>,<Luma:(3.0)>]
--
toUnboxedVector :: Array VU cs e => Image VU cs e -> Vector (Pixel cs e)
toUnboxedVector (VUImage _ _ v) = v
toUnboxedVector (VScalar px) = V.singleton px


-- | Construct a two dimensional image with @m@ rows and @n@ columns from a flat
-- Unboxed 'Vector' of length @k@. It is a __O(1)__ opeartion. Make sure that @m * n = k@.
--
-- >>> fromUnboxedVector (200, 300) $ generate 60000 (\i -> PixelY $ fromIntegral i / 60000)
-- <Image VectorUnboxed Luma: 200x300>
--
-- <<images/grad_fromVector.png>>
-- 
fromUnboxedVector :: Array VU cs e => (Int, Int) -> Vector (Pixel cs e) -> Image VU cs e
fromUnboxedVector (m, n) v
  | m * n == V.length v = VUImage m n v
  | otherwise = error "fromUnboxedVector: m * n doesn't equal the length of a Vector."


-- | 2D index conversion to a flat vector index.
fromIx :: Int -> (Int, Int) -> Int
fromIx !n !(i, j) = i * n + j
{-# INLINE fromIx #-}


-- | Vector to 2D index conversion.
toIx :: Int -> Int -> (Int, Int)
toIx !n !k = (k `div` n, k `mod` n)
{-# INLINE toIx #-}

