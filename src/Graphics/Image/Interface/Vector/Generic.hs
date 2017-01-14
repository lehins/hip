{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Generic
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Generic (
  V(..), Repr, Image(..), fromVector, toVector
  ) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P (map)
import Control.DeepSeq (NFData, deepseq)
import Control.Monad
import Control.Monad.ST
import Data.Foldable (forM_)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor
#endif
import Data.Primitive.MutVar
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as MVG
import Graphics.Image.Interface as I

-- | Generic 'Vector' representation.
data V r = V r

type family Repr arr :: * -> *

instance Show r => Show (V r) where
  show r = "Vector " ++ show r

instance SuperClass (V r) cs e => BaseArray (V r) cs e where
  type SuperClass (V r) cs e = (Show r, ColorSpace cs, Num e, Typeable e,
                                VG.Vector (Repr (V r)) (Pixel cs e), VG.Vector (Repr (V r)) Int,
                                VG.Vector (Repr (V r)) Bool, NFData ((Repr (V r)) (Pixel cs e)))

  data Image (V r) cs e = VScalar !(Pixel cs e)
                        | VImage {-# UNPACK #-} !Int
                                 {-# UNPACK #-} !Int
                                 !((Repr (V r)) (Pixel cs e))

  dims (VImage m n _) = (m, n)
  dims (VScalar _)    = (1, 1)
  {-# INLINE dims #-}


instance (MArray (V r) cs e, BaseArray (V r) cs e) => Array (V r) cs e where

  type Manifest (V r) = V r

  makeImage !(checkDims "(V r).makeImage" -> (m, n)) f =
    VImage m n $ VG.generate (m * n) (f . toIx n)
  {-# INLINE makeImage #-}

  makeImageWindowed sz !((it, jt), (ib, jb)) getWindowPx getBorderPx =
    VImage m n $ VG.create generate where
      !(m, n) = checkDims "(V r).makeImageWindowed" sz
      nestedLoop !mv !getPx !fi !fj !ti !tj = do
        forM_ [fi .. ti-1] $ \i ->
          forM_ [fj .. tj-1] $ \j ->
            MVG.write mv (fromIx n (i,j)) (getPx (i, j))
      {-# INLINE nestedLoop #-}
      generate :: ST s ((VG.Mutable (Repr (V r))) s (Pixel cs e))
      generate = do
        mv <- MVG.unsafeNew (m*n)
        nestedLoop mv getBorderPx 0 0 ib n
        nestedLoop mv getBorderPx it 0 ib jt
        nestedLoop mv getWindowPx it jt ib jb
        nestedLoop mv getBorderPx it jb ib n
        nestedLoop mv getBorderPx ib 0 m n
        return mv
      {-# INLINE generate #-}
  {-# INLINE makeImageWindowed #-}
  
  singleton = VScalar
  {-# INLINE singleton #-}

  index00 (VScalar px) = px
  index00 (VImage _ _ v) = v VG.! 0
  {-# INLINE index00 #-}
  
  map f (VScalar px)    = VScalar (f px)
  map f (VImage m n v) = VImage m n (VG.map f v)
  {-# INLINE map #-}

  imap f (VScalar px)    = VScalar (f (0, 0) px)
  imap f (VImage m n v) = VImage m n (VG.imap (\ !k !px -> f (toIx n k) px) v)
  {-# INLINE imap #-}
  
  zipWith f (VScalar px1) (VScalar px2)    = VScalar (f px1 px2)
  zipWith f (VScalar px1) (VImage m n v2) = VImage m n (VG.map (f px1) v2)
  zipWith f (VImage m n v1) (VScalar px2) = VImage m n (VG.map (`f` px2) v1)
  zipWith f img1@(VImage m1 n1 v1) img2@(VImage m2 n2 v2) =
    if m1 /= m2 || n1 /= n2
    then error ("zipWith: Images must be of the same dimensions, received: "++
                show img1++" and "++show img2++".")
    else VImage m1 n1 (VG.zipWith f v1 v2)
  {-# INLINE zipWith #-}

  izipWith f (VScalar px1) (VScalar px2)    = VScalar (f (0, 0) px1 px2)
  izipWith f (VScalar px1) (VImage m n v2) =
    VImage m n (VG.imap (\ !k !px2 -> f (toIx n k) px1 px2) v2)
  izipWith f (VImage m n v1) (VScalar px2) =
    VImage m n (VG.imap (\ !k !px1 -> f (toIx n k) px1 px2) v1)
  izipWith f img1@(VImage m1 n1 v1) img2@(VImage m2 n2 v2) =
    if m1 /= m2 || n1 /= n2
    then error ("izipWith: Images must be of the same dimensions, received: "++
                show img1++" and "++show img2++".")
    else VImage m1 n1 (VG.izipWith (\ !k !px1 !px2 -> f (toIx n1 k) px1 px2) v1 v2)
  {-# INLINE izipWith #-}

  traverse !img getNewDims getNewPx = makeImage (getNewDims (dims img)) (getNewPx (index img))
  {-# INLINE traverse #-}

  traverse2 !img1 !img2 getNewDims getNewPx =
    makeImage (getNewDims (dims img1) (dims img2)) (getNewPx (index img1) (index img2))
  {-# INLINE traverse2 #-}

  -- TODO: switch directly to VG.unsafeBackpermute (no need to check ixs)
  transpose !img = backpermute (n, m) movePx img where
    !(m, n) = dims img
    movePx !(i, j) = (j, i)
    {-# INLINE movePx #-}
  {-# INLINE transpose #-}

  -- TODO: add index verification and switch to VG.unsafeBackpermute
  backpermute !(checkDims "(V r).backpermute" -> (m, n)) !f (VImage _ n' v) =
    VImage m n $ VG.backpermute v $ VG.generate (m*n) (fromIx n' . f . toIx n)
  backpermute !sz _ (VScalar px) = makeImage sz (const px)
  {-# INLINE backpermute #-}
  
  fromLists !ls = if all (== n) (P.map length ls)
                  then VImage m n . VG.fromList . concat $ ls
                  else error "fromLists: Inner lists are of different lengths."
    where
      !(m, n) = checkDims "(V r).fromLists" (length ls, length $ head ls)
  {-# INLINE fromLists #-}

  fold !f !px0 (VImage _ _ v) = VG.foldl' f px0 v
  fold !f !px0 (VScalar px)    = f px0 px
  {-# INLINE fold #-}

  foldIx !f !px0 (VImage _ n v) = VG.ifoldl' f' px0 v where
    f' !acc !k !px = f acc (toIx n k) px
  foldIx !f !px0 (VScalar px)    = f px0 (0,0) px
  {-# INLINE foldIx #-}

  (|*|) img1@(VImage m1 n1 v1) !img2@VImage {} =
    if n1 /= m2 
    then error ("Inner dimensions of multiplying images must be the same, but received: "++
                show img1 ++" X "++ show img2)
    else
      makeImage (m1, n2) getPx where
        VImage n2 m2 v2 = transpose img2
        getPx !(i, j) = VG.sum $ VG.zipWith (*) (VG.slice (i*n1) n1 v1) (VG.slice (j*m2) m2 v2)
        {-# INLINE getPx #-}
  (|*|) (VScalar px1) (VScalar px2) = VScalar (px1 * px2)
  (|*|) _ _ = error "Scalar Images cannot be multiplied."
  {-# INLINE (|*|) #-}

  eq (VImage m1 n1 v1) (VImage m2 n2 v2) =
    m1 == m2 && n1 == n2 && VG.all id (VG.zipWith (==) v1 v2)
  eq (VScalar px1)           (VScalar px2) = px1 == px2
  eq (VImage 1 1 v1) (VScalar px2) = v1 VG.! 0 == px2
  eq (VScalar px1) (VImage 1 1 v2) = v2 VG.! 0 == px1
  eq _ _ = False
  {-# INLINE eq #-}

  compute (VImage m n v) = m `seq` n `seq` v `deepseq` (VImage m n v)
  compute (VScalar px)    = px `seq` (VScalar px)
  {-# INLINE compute #-}

  toManifest = id
  {-# INLINE toManifest #-}


instance BaseArray (V r) cs e => MArray (V r) cs e where
  
  data MImage s (V r) cs e = MVImage !Int !Int ((VG.Mutable (Repr (V r))) s (Pixel cs e))
                            | MVScalar (MutVar s (Pixel cs e))
                              

  unsafeIndex (VImage _ n v) !ix = VG.unsafeIndex v (fromIx n ix)
  unsafeIndex (VScalar px)     _ = px
  {-# INLINE unsafeIndex #-}

  deepSeqImage (VImage m n v) = m `seq` n `seq` deepseq v
  deepSeqImage (VScalar px)   = seq px
  {-# INLINE deepSeqImage #-}

  foldl f !a (VImage _ _ v) = VG.foldl' f a v
  foldl f !a (VScalar px)   = f a px
  {-# INLINE foldl #-}

  foldr f !a (VImage _ _ v) = VG.foldr' f a v
  foldr f !a (VScalar px)   = f px a
  {-# INLINE foldr #-}

  makeImageM !(checkDims "(V r).makeImageM" -> (m, n)) !f =
    VImage m n <$> VG.generateM (m * n) (f . toIx n)
  {-# INLINE makeImageM #-}

  mapM f (VImage m n v) = VImage m n <$> VG.mapM f v
  mapM f (VScalar px)   = VScalar <$> f px
  {-# INLINE mapM #-}

  mapM_ f (VImage _ _ v) = VG.mapM_ f v
  mapM_ f (VScalar px)    = void $ f px
  {-# INLINE mapM_ #-}

  foldM f !a (VImage _ _ v) = VG.foldM' f a v
  foldM f !a (VScalar px)    = f a px
  {-# INLINE foldM #-}

  foldM_ f !a (VImage _ _ v) = VG.foldM'_ f a v
  foldM_ f !a (VScalar px)    = void $ f a px
  {-# INLINE foldM_ #-}


  mdims (MVImage m n _) = (m, n)
  mdims (MVScalar _)    = (1, 1)
  {-# INLINE mdims #-}

  thaw (VImage m n v) = MVImage m n <$> VG.thaw v
  thaw (VScalar px)   = MVScalar <$> newMutVar px
  {-# INLINE thaw #-}

  freeze (MVImage m n mv) = VImage m n <$> VG.freeze mv
  freeze (MVScalar mpx)    = VScalar <$> readMutVar mpx
  {-# INLINE freeze #-}

  new (m, n) = MVImage m n <$> MVG.new (m*n)
  {-# INLINE new #-}

  read (MVImage _ n mv) !ix = MVG.read mv (fromIx n ix)
  read (MVScalar mpx)   !ix = do
    unless ((0, 0) == ix) $ error $ "Index out of bounds: " ++ show ix
    readMutVar mpx
  {-# INLINE read #-}

  write (MVImage _ n mv) !ix !px = MVG.write mv (fromIx n ix) px
  write (MVScalar mv)    !ix !px = do
    unless ((0, 0) == ix) $ error $ "Index out of bounds: " ++ show ix
    writeMutVar mv px
  {-# INLINE write #-}

  swap (MVImage _ n mv) !ix1 !ix2 = MVG.swap mv (fromIx n ix1) (fromIx n ix2)
  swap _                _    _    = return ()
  {-# INLINE swap #-}


-- | Convert an image to a flattened  'Vector'. It is a __O(1)__ opeartion.
--
-- >>> toVector $ makeImage (3, 2) (\(i, j) -> PixelY $ fromIntegral (i+j))
-- fromList [<Luma:(0.0)>,<Luma:(1.0)>,<Luma:(1.0)>,<Luma:(2.0)>,<Luma:(2.0)>,<Luma:(3.0)>]
--
toVector :: VG.Vector (Repr (V r)) (Pixel cs e) => Image (V r) cs e -> (Repr (V r)) (Pixel cs e)
toVector (VImage _ _ v) = v
toVector (VScalar px)   = VG.singleton px
{-# INLINE toVector #-}


-- | Construct a two dimensional image with @m@ rows and @n@ columns from a flat
--  'Vector' of length @k@. It is a __O(1)__ opeartion. Make sure that @m * n = k@.
--
-- >>> fromVector (200, 300) $ generate 60000 (\i -> PixelY $ fromIntegral i / 60000)
-- <Image Vector Luma: 200x300>
--
-- <<images/grad_fromVector.png>>
-- 
fromVector :: (Array (V r) cs e, VG.Vector (Repr (V r)) (Pixel cs e)) => (Int, Int) -> (Repr (V r)) (Pixel cs e) -> Image (V r) cs e
fromVector !(m, n) !v
  | m * n == VG.length v = VImage m n v
  | otherwise = error "fromVector: m * n doesn't equal the length of a Vector."
{-# INLINE fromVector #-}
