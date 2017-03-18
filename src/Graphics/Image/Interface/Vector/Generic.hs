{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Generic
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Generic
  ( VGImage
  , MVGImage
  , makeImageVG
  , dimsVG
  , scalarVG
  , index00VG
  , makeImageWindowedVG
  , mapVG
  , imapVG
  , zipWithVG
  , izipWithVG
  , unsafeTraverseVG
  , traverseVG
  , unsafeTraverse2VG
  , traverse2VG
  , transposeVG
  , unsafeBackpermuteVG
  , backpermuteVG
  , fromListsVG
  , foldlVG
  , foldrVG
  , ifoldlVG
  , toVectorVG
  , fromVectorVG
  , multVG
  , unsafeIndexVG
  , makeImageMVG
  , mapMVG
  , mapM_VG
  , foldMVG
  , foldM_VG
  , mdimsVG
  , thawVG
  , freezeVG
  , newVG
  , readVG
  , writeVG
  , swapVG
  )
where

import           Control.DeepSeq             (NFData (rnf))
import           Control.Monad               (when)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (ST)
import           Prelude                     hiding (map, zipWith)
#if !MIN_VERSION_base(4,8,0)
import           Data.Functor
#endif
import           Data.Maybe                  (listToMaybe)
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as MVG
import           Graphics.Image.Interface    (checkDims, fromIx,
                                              handleBorderIndex, toIx)
import           Graphics.Image.Utils        (loopM_, swapIx)


data VGImage v p =
  VGImage {-# UNPACK #-}!Int
          {-# UNPACK #-}!Int
          !(v p)

instance NFData (v p) => NFData (VGImage v p) where
  rnf (VGImage _ _ v) = rnf v
  {-# INLINE rnf #-}

instance Eq (v p) => Eq (VGImage v p) where
  (VGImage _ _ v1) == (VGImage _ _ v2) = v1 == v2
  {-# INLINE (==) #-}

makeImageVG :: VG.Vector v p =>
               (Int, Int) -> ((Int, Int) -> p) -> VGImage v p
makeImageVG !sz f =
  let !(m, n) = checkDimsVG "makeImageVGM" sz in
    VGImage m n $ VG.generate (m * n) (f . toIx n)
{-# INLINE makeImageVG #-}

dimsVG :: VGImage v p -> (Int, Int)
dimsVG (VGImage m n _) = (m, n)
{-# INLINE dimsVG #-}


scalarVG :: VG.Vector v p => p -> VGImage v p
scalarVG = makeImageVG (1,1) . const
{-# INLINE scalarVG #-}

index00VG :: VG.Vector v p => VGImage v p -> p
index00VG (VGImage _ _ v) = v VG.! 0
{-# INLINE index00VG #-}

-- makeImageVG
--   :: forall v p.
--      VG.Vector v p
--   => (Int, Int)
--   -> ((Int, Int) -> p)
--   -> VGImage v p
-- makeImageVG !sz getPx =
--   VGImage m n $ VG.create generateImage
--   where
--     !(m, n) = checkDimsVG "makeImageVG" sz
--     generateImage :: ST s ((VG.Mutable v) s p)
--     generateImage = do
--       mv <- MVG.unsafeNew (m * n)
--       loopM_ 0 (< m) (+ 1) $ \ !i -> do
--         loopM_ 0 (< n) (+ 1) $ \ !j -> do
--           MVG.unsafeWrite mv (fromIx n (i, j)) (getPx (i, j))
--       return mv
--     {-# INLINE generateImage #-}
-- {-# INLINE makeImageVG #-}


makeImageWindowedVG
  :: forall v p.
     VG.Vector v p
  => (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> VGImage v p
makeImageWindowedVG !sz !(it, jt) !(wm, wn) getWindowPx getBorderPx =
  VGImage m n $ VG.create generate
  where
    !(ib, jb) = (wm + it, wn + jt)
    !(m, n) = checkDimsVG "makeImageWindowedVG" sz
    !_ = checkDimsVG "makeImageWindowedVG (window size)" (wm, wn)
    generate :: ST s ((VG.Mutable v) s p)
    generate = do
      when (it < 0 || it >= ib || jt < 0 || jt >= jb || ib > m || jb > n) $
        error
          ("Window index is outside the image dimensions. window start: " ++
           show (it, jt) ++
           " window size: " ++
           show (wm, wn) ++ " image dimensions: " ++ show (m, n))
      mv <- MVG.unsafeNew (m * n)
      loopM_ 0 (< n) (+ 1) $ \ !j -> do
        loopM_ 0 (< it) (+ 1) $ \ !i -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
        loopM_ ib (< m) (+ 1) $ \ !i -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
      loopM_ it (< ib) (+ 1) $ \ !i -> do
        loopM_ 0 (< jt) (+ 1) $ \ !j -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
        loopM_ jt (< jb) (+ 1) $ \ !j -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getWindowPx (i, j))
        loopM_ jb (< n) (+ 1) $ \ !j -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
      return mv
    {-# INLINE generate #-}
{-# INLINE makeImageWindowedVG #-}




mapVG :: (VG.Vector v p1, VG.Vector v p)
      => (p1 -> p) -> VGImage v p1 -> VGImage v p
mapVG f (VGImage m n v) = VGImage m n (VG.map f v)
{-# INLINE mapVG #-}

imapVG :: (VG.Vector v p1, VG.Vector v p)
       => ((Int, Int) -> p1 -> p) -> VGImage v p1 -> VGImage v p
imapVG f (VGImage m n v) = VGImage m n (VG.imap (\ !k !px -> f (toIx n k) px) v)
{-# INLINE imapVG #-}

zipWithVG
  :: ( VG.Vector v p1
     , VG.Vector v p2
     , VG.Vector v p
     )
  => (p1 -> p2 -> p)
  -> VGImage v p1
  -> VGImage v p2
  -> VGImage v p
zipWithVG f (VGImage 1 1 v1) (VGImage m n v2) =
  let !px1 = VG.unsafeIndex v1 0
  in VGImage m n (VG.map (f px1) v2)
zipWithVG f (VGImage m n v1) (VGImage 1 1 v2) =
  let !px2 = VG.unsafeIndex v2 0
  in VGImage m n (VG.map (`f` px2) v1)
zipWithVG f (VGImage m1 n1 v1) (VGImage m2 n2 v2) =
  if m1 == m2 || n1 == n2
    then VGImage m1 n1 (VG.zipWith f v1 v2)
    else let !(m, n) = (min m1 m2, min n1 n2)
             getPx !k =
               let !ix = toIx n k
                   !px1 = VG.unsafeIndex v1 (fromIx n1 ix)
                   !px2 = VG.unsafeIndex v2 (fromIx n2 ix)
               in f px1 px2
         in VGImage m n $ VG.generate (m * n) getPx
{-# INLINE zipWithVG #-}


izipWithVG
  :: (VG.Vector v p1, VG.Vector v p2, VG.Vector v p)
  => ((Int, Int) -> p1 -> p2 -> p)
  -> VGImage v p1
  -> VGImage v p2
  -> VGImage v p
izipWithVG f (VGImage 1 1 v1) (VGImage m n v2) =
  let !px1 = VG.unsafeIndex v1 0
  in VGImage m n (VG.imap (\ !k !px2 -> f (toIx n k) px1 px2) v2)
izipWithVG f (VGImage m n v1) (VGImage 1 1 v2) =
  let !px2 = VG.unsafeIndex v2 0
  in VGImage m n (VG.imap (\ !k !px1 -> f (toIx n k) px1 px2) v1)
izipWithVG f (VGImage m1 n1 v1) (VGImage m2 n2 v2) =
  if m1 == m2 || n1 == n2
    then VGImage m1 n1 (VG.izipWith (\ !k !px1 !px2 -> f (toIx n1 k) px1 px2) v1 v2)
    else let !(m, n) = (min m1 m2, min n1 n2)
             getPx !k =
               let !ix = toIx n k
                   !px1 = VG.unsafeIndex v1 (fromIx n1 ix)
                   !px2 = VG.unsafeIndex v2 (fromIx n2 ix)
               in f ix px1 px2
         in VGImage m n $ VG.generate (m * n) getPx
{-# INLINE izipWithVG #-}


unsafeIndexV :: VG.Vector v a => Int -> v a -> (Int, Int) -> a
unsafeIndexV !n !v !ix = let !k = fromIx n ix in VG.unsafeIndex v k
{-# INLINE unsafeIndexV #-}

indexV :: VG.Vector v p => (Int, Int) -> v p -> (Int, Int) -> p
indexV !sz@(_, n) !v !ix =
  handleBorderIndex
    (errorVG "indexV" ("Index out of bounds <" ++ show sz ++ ">: " ++ show ix))
    sz
    (unsafeIndexV n v)
    ix
{-# INLINE indexV #-}



unsafeTraverseVG
  :: (VG.Vector v p1, VG.Vector v p)
  => VGImage v p1
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> (Int, Int) -> p)
  -> VGImage v p
unsafeTraverseVG (VGImage m n v) getNewDims getNewPx =
  makeImageVG (getNewDims (m, n)) (getNewPx (unsafeIndexV n v))
{-# INLINE unsafeTraverseVG #-}


traverseVG
  :: (VG.Vector v p1, VG.Vector v p)
  => VGImage v p1
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> (Int, Int) -> p)
  -> VGImage v p
traverseVG (VGImage m n v) getNewDims getNewPx =
  makeImageVG (getNewDims (m, n)) (getNewPx (indexV (m, n) v))
{-# INLINE traverseVG #-}


unsafeTraverse2VG
  :: (VG.Vector v p1, VG.Vector v p2, VG.Vector v p)
  => VGImage v p1
  -> VGImage v p2
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> ((Int, Int) -> p2) -> (Int, Int) -> p)
  -> VGImage v p
unsafeTraverse2VG (VGImage m1 n1 v1) (VGImage m2 n2 v2) getNewDims getNewPx =
  makeImageVG
    (getNewDims (m1, n1) (m2, n2))
    (getNewPx (unsafeIndexV n1 v1) (unsafeIndexV n2 v2))
{-# INLINE unsafeTraverse2VG #-}


traverse2VG
  :: (VG.Vector v p1, VG.Vector v p2, VG.Vector v p)
  => VGImage v p1
  -> VGImage v p2
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> ((Int, Int) -> p2) -> (Int, Int) -> p)
  -> VGImage v p
traverse2VG (VGImage m1 n1 v1) (VGImage m2 n2 v2) getNewDims getNewPx =
  makeImageVG
    (getNewDims (m1, n1) (m2, n2))
    (getNewPx (indexV (m1, n1) v1) (indexV (m2, n2) v2))
{-# INLINE traverse2VG #-}


transposeVG :: (VG.Vector v Int, VG.Vector v p) =>
               VGImage v p -> VGImage v p
transposeVG (VGImage m n v) =
  VGImage n m $ VG.unsafeBackpermute v $ VG.generate (m * n) (fromIx n . swapIx . toIx m)
{-# INLINE transposeVG #-}

backpermuteWithCheckVG
  :: (VG.Vector v Int, VG.Vector v p)
  => ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> VGImage v p
  -> VGImage v p
backpermuteWithCheckVG checkIx !sz f (VGImage m' n' v) =
  let !(m, n) = checkDimsVG "backpermuteWithCheckVG" sz
  in VGImage m n $
     VG.unsafeBackpermute v $
     VG.generate (m * n) (fromIx n' . checkIx (m', n') . f . toIx n)
{-# INLINE backpermuteWithCheckVG #-}


unsafeBackpermuteVG
  :: (VG.Vector v Int, VG.Vector v p)
  => (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> VGImage v p
  -> VGImage v p
unsafeBackpermuteVG = backpermuteWithCheckVG (const id)
{-# INLINE unsafeBackpermuteVG #-}


backpermuteVG
  :: (VG.Vector v Int, VG.Vector v p)
  => (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> VGImage v p
  -> VGImage v p
backpermuteVG =
  backpermuteWithCheckVG $ \ !sz !ix ->
    let err =
          errorVG
            "backpermuteVG"
            ("Index out of bounds <" ++ show sz ++ ">: " ++ show ix)
    in handleBorderIndex err sz id ix
{-# INLINE backpermuteVG #-}


fromListsVG :: VG.Vector v p => [[p]] -> VGImage v p
fromListsVG !ls =
  if all (== n) (fmap length ls)
    then VGImage m n . VG.fromList . concat $ ls
    else errorVG "fromListsVG" "Inner lists are of different lengths."
  where
    (m, n) =
      checkDimsVG "fromListsVG" (length ls, maybe 0 length $ listToMaybe ls)
{-# NOINLINE fromListsVG #-}

foldlVG :: VG.Vector v p =>
           (a -> p -> a) -> a -> VGImage v p -> a
foldlVG !f !px0 (VGImage _ _ v) = VG.foldl' f px0 v
{-# INLINE foldlVG #-}

foldrVG :: VG.Vector v p =>
           (p -> a -> a) -> a -> VGImage v p -> a
foldrVG !f !px0 (VGImage _ _ v) = VG.foldr' f px0 v
{-# INLINE foldrVG #-}


ifoldlVG :: VG.Vector v p =>
            (a -> (Int, Int) -> p -> a) -> a -> VGImage v p -> a
ifoldlVG !f !px0 (VGImage _ n v) =
  VG.ifoldl' (\ !acc !k -> f acc (toIx n k)) px0 v
{-# INLINE ifoldlVG #-}


toVectorVG :: (VG.Vector v p)
           => VGImage v p -> v p
toVectorVG (VGImage _ _ v) = v
{-# INLINE toVectorVG #-}

fromVectorVG :: VG.Vector v p =>
                (Int, Int) -> v p -> VGImage v p
fromVectorVG !(m, n) !v
  | m * n == VG.length v = VGImage m n v
  | otherwise =
    errorVG "fromVectorVG" $
    " image dimensions do not match the length of a vector: " ++
    show m ++ " * " ++ show n ++ " /= " ++ show (VG.length v)
{-# INLINE fromVectorVG #-}

multVG :: ( VG.Vector v Int
          , VG.Vector v p
          , Num p
          ) => VGImage v p -> VGImage v p -> VGImage v p
multVG (VGImage m1 n1 v1) img2 =
  if n1 /= m2
    then errorVG "multVG" $
         "Inner dimensions of images must agree, but received: " ++
         show (m1, n1) ++ " X " ++ show (m2, n2)
    else makeImageVG (m1, n2) getPx
  where
    VGImage n2 m2 v2 = transposeVG img2
    getPx !(i, j) =
      VG.sum $
      VG.zipWith (*) (VG.slice (i * n1) n1 v1) (VG.slice (j * m2) m2 v2)
    {-# INLINE getPx #-}
{-# INLINE multVG #-}



------ Manifest


data MVGImage s v p = MVGImage !Int !Int (VG.Mutable v s p)


unsafeIndexVG :: VG.Vector v p => VGImage v p -> (Int, Int) -> p
unsafeIndexVG (VGImage _ n v) = VG.unsafeIndex v . fromIx n
{-# INLINE unsafeIndexVG #-}


makeImageMVG :: (Monad m, VG.Vector v p) =>
                (Int, Int) -> ((Int, Int) -> m p) -> m (VGImage v p)
makeImageMVG !sz !f =
  let !(m, n) = checkDimsVG "makeImageMVG" sz in
    VGImage m n <$> VG.generateM (m * n) (f . toIx n)
{-# INLINE makeImageMVG #-}

mapMVG
  :: (Monad m, VG.Vector v a, VG.Vector v p) =>
     (a -> m p) -> VGImage v a -> m (VGImage v p)
mapMVG f (VGImage m n v) = VGImage m n <$> VG.mapM f v
{-# INLINE mapMVG #-}

mapM_VG
  :: (Monad m, VG.Vector v a) => (a -> m b) -> VGImage v a -> m ()
mapM_VG f (VGImage _ _ v) = VG.mapM_ f v
{-# INLINE mapM_VG #-}

foldMVG
  :: (Monad m, VG.Vector v b) =>
     (a -> b -> m a) -> a -> VGImage v b -> m a
foldMVG f !a (VGImage _ _ v) = VG.foldM' f a v
{-# INLINE foldMVG #-}

foldM_VG
  :: (Monad m, VG.Vector v b) =>
     (a -> b -> m a) -> a -> VGImage v b -> m ()
foldM_VG f !a (VGImage _ _ v) = VG.foldM'_ f a v
{-# INLINE foldM_VG #-}


mdimsVG :: MVGImage t2 t1 t -> (Int, Int)
mdimsVG (MVGImage m n _) = (m, n)
{-# INLINE mdimsVG #-}

thawVG :: (VG.Mutable v1 ~ VG.Mutable v, PrimMonad f, VG.Vector v1 p) =>
     VGImage v1 p -> f (MVGImage (PrimState f) v p)
thawVG (VGImage m n v) = MVGImage m n <$> VG.thaw v
{-# INLINE thawVG #-}

freezeVG :: (VG.Mutable t ~ VG.Mutable v, PrimMonad f, VG.Vector v p) =>
     MVGImage (PrimState f) t p -> f (VGImage v p)
freezeVG (MVGImage m n mv) = VGImage m n <$> VG.freeze mv
{-# INLINE freezeVG #-}

newVG :: (PrimMonad f, MVG.MVector (VG.Mutable v) p) =>
     (Int, Int) -> f (MVGImage (PrimState f) v p)
newVG (m, n) = MVGImage m n <$> MVG.new (m*n)
{-# INLINE newVG #-}

readVG :: (PrimMonad m, MVG.MVector (VG.Mutable t) a) =>
     MVGImage (PrimState m) t a -> (Int, Int) -> m a
readVG (MVGImage _ n mv) !ix = MVG.read mv (fromIx n ix)
{-# INLINE readVG #-}

writeVG :: (PrimMonad m, MVG.MVector (VG.Mutable t) a) =>
     MVGImage (PrimState m) t a -> (Int, Int) -> a -> m ()
writeVG (MVGImage _ n mv) !ix !px = MVG.write mv (fromIx n ix) px
{-# INLINE writeVG #-}

swapVG :: (PrimMonad m, MVG.MVector (VG.Mutable v) p) =>
           MVGImage (PrimState m) v p -> (Int, Int) -> (Int, Int) -> m ()
swapVG (MVGImage _ n mv) !ix1 !ix2 = MVG.swap mv (fromIx n ix1) (fromIx n ix2)
{-# INLINE swapVG #-}


errorVG :: String -> String -> a
errorVG fName errMsg =
  error $ "Graphics.Image.Interface.Vector.Generic." ++ fName ++ ": " ++ errMsg



checkDimsVG :: String -> (Int, Int) -> (Int, Int)
checkDimsVG fName = checkDims ("Graphics.Image.Interface.Vector.Generic." ++ fName)
{-# INLINE checkDimsVG #-}
