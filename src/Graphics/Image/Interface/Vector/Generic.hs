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
  ( VGArray
  , MVGArray
  , makeArrayVG
  , dimsVG
  , scalarVG
  , index00VG
  , indexVG
  , makeArrayWindowedVG
  , makeArrayWindowedVGPar
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
  , makeArrayMVG
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
import           Graphics.Image.Internal     (handleBorderIndex)
import           Graphics.Image.Utils        (checkDims, fromIx, loopM_, swapIx,
                                              toIx)

import Data.Array.Repa.Eval.Gang


data VGArray v p =
  VGArray {-# UNPACK #-}!Int
          {-# UNPACK #-}!Int
          !(v p)

instance NFData (v p) => NFData (VGArray v p) where
  rnf (VGArray _ _ v) = rnf v
  {-# INLINE rnf #-}

instance Eq (v p) => Eq (VGArray v p) where
  (VGArray _ _ v1) == (VGArray _ _ v2) = v1 == v2
  {-# INLINE (==) #-}

makeArrayVG :: VG.Vector v p =>
               (Int, Int) -> ((Int, Int) -> p) -> VGArray v p
makeArrayVG !sz f =
  let !(m, n) = checkDimsVG "makeArrayVGM" sz in
    VGArray m n $ VG.generate (m * n) (f . toIx n)
{-# INLINE makeArrayVG #-}

dimsVG :: VGArray v p -> (Int, Int)
dimsVG (VGArray m n _) = (m, n)
{-# INLINE dimsVG #-}


scalarVG :: VG.Vector v p => p -> VGArray v p
scalarVG = makeArrayVG (1,1) . const
{-# INLINE scalarVG #-}

index00VG :: VG.Vector v p => VGArray v p -> p
index00VG (VGArray _ _ v) = v VG.! 0
{-# INLINE index00VG #-}

-- makeArrayVG
--   :: forall v p.
--      VG.Vector v p
--   => (Int, Int)
--   -> ((Int, Int) -> p)
--   -> VGArray v p
-- makeArrayVG !sz getPx =
--   VGArray m n $ VG.create generateArray
--   where
--     !(m, n) = checkDimsVG "makeArrayVG" sz
--     generateArray :: ST s ((VG.Mutable v) s p)
--     generateArray = do
--       mv <- MVG.unsafeNew (m * n)
--       loopM_ 0 (< m) (+ 1) $ \ !i -> do
--         loopM_ 0 (< n) (+ 1) $ \ !j -> do
--           MVG.unsafeWrite mv (fromIx n (i, j)) (getPx (i, j))
--       return mv
--     {-# INLINE generateArray #-}
-- {-# INLINE makeArrayVG #-}


makeVectorWindowedPar
  :: forall v p.
     VG.Vector v p
  => (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> v p
makeVectorWindowedPar !(m, n) !(it, jt) !(wm, wn) getWindowPx getBorderPx =
  VG.create generate
  where
    !(ib, jb) = checkWindow m n it jt wm wn
    generate :: ST s ((VG.Mutable v) s p)
    generate = do
      mv <- MVG.unsafeNew (m * n)
      loopM_ 0 (< n) (+ 1) $ \ !j -> do
        loopM_ 0 (< it) (+ 1) $ \ !i -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
        loopM_ ib (< m) (+ 1) $ \ !i -> do
          MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
      let !gSize = gangSize theGang
          !(chunkHeight, slackHeight) = wm `divMod` gSize
      let loadRows !it' !ib' = do
            loopM_ it' (< ib') (+ 1) $ \ !i -> do
              loopM_ 0 (< jt) (+ 1) $ \ !j -> do
                MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
              loopM_ jt (< jb) (+ 1) $ \ !j -> do
                MVG.unsafeWrite mv (fromIx n (i, j)) (getWindowPx (i, j))
              loopM_ jb (< n) (+ 1) $ \ !j -> do
                MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
          {-# INLINE loadRows #-}
      gangST theGang $ \ !cix -> do
        let !it' = cix * chunkHeight + it
        loadRows it' (it' + chunkHeight)
        when (cix == 0) $ do
          loopM_ 0 (< n) (+ 1) $ \ !j -> do
            loopM_ 0 (< it) (+ 1) $ \ !i -> do
              MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
        when (cix == 1 `mod` gSize) $ do
          loopM_ 0 (< n) (+ 1) $ \ !j -> do
            loopM_ ib (< m) (+ 1) $ \ !i -> do
              MVG.unsafeWrite mv (fromIx n (i, j)) (getBorderPx (i, j))
        when (cix == 2 `mod` gSize && slackHeight > 0) $ do
          let !itSlack = gSize * chunkHeight + it
          loadRows itSlack (itSlack + slackHeight)
      return mv
    {-# INLINE generate #-}
{-# INLINE makeVectorWindowedPar #-}


makeArrayWindowedVGPar
  :: VG.Vector v p
  => (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> VGArray v p
makeArrayWindowedVGPar !(m, n) !wIx !wSz getWindowPx getBorderPx =
  VGArray m n $ makeVectorWindowedPar (m, n) wIx wSz getWindowPx getBorderPx
{-# INLINE makeArrayWindowedVGPar #-}


-- | Checks window and array dimensions and returns index for the bottom right
-- corner of the window.
checkWindow :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
checkWindow !m !n !it !jt !wm !wn
  | it < 0 || it >= ib || jt < 0 || jt >= jb || ib > m || jb > n =
    errorVG
      "checkWindow"
      ("Window index is outside the array dimensions. window start: " ++
       show (it, jt) ++
       " window size: " ++ show (wm, wn) ++ " array dimensions: " ++ show (m, n))
  | otherwise = (ib, jb)
  where
    !(ib, jb) = (wm + it, wn + jt)
    !_ = checkDimsVG "checkWindow (array size)" (m, n)
    !_ = checkDimsVG "checkWindow (window size)" (wm, wn)


makeArrayWindowedVG
  :: forall v p.
     VG.Vector v p
  => (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> VGArray v p
makeArrayWindowedVG !(m, n) !(it, jt) !(wm, wn) getWindowPx getBorderPx =
  VGArray m n $ VG.create generate
  where
    !(ib, jb) = checkWindow m n it jt wm wn
    generate :: ST s ((VG.Mutable v) s p)
    generate = do
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
{-# INLINE makeArrayWindowedVG #-}




mapVG :: (VG.Vector v p1, VG.Vector v p)
      => (p1 -> p) -> VGArray v p1 -> VGArray v p
mapVG f (VGArray m n v) = VGArray m n (VG.map f v)
{-# INLINE mapVG #-}

imapVG :: (VG.Vector v p1, VG.Vector v p)
       => ((Int, Int) -> p1 -> p) -> VGArray v p1 -> VGArray v p
imapVG f (VGArray m n v) = VGArray m n (VG.imap (\ !k !px -> f (toIx n k) px) v)
{-# INLINE imapVG #-}

zipWithVG
  :: ( VG.Vector v p1
     , VG.Vector v p2
     , VG.Vector v p
     )
  => (p1 -> p2 -> p)
  -> VGArray v p1
  -> VGArray v p2
  -> VGArray v p
zipWithVG f (VGArray 1 1 v1) (VGArray m n v2) =
  let !px1 = VG.unsafeIndex v1 0
  in VGArray m n (VG.map (f px1) v2)
zipWithVG f (VGArray m n v1) (VGArray 1 1 v2) =
  let !px2 = VG.unsafeIndex v2 0
  in VGArray m n (VG.map (`f` px2) v1)
zipWithVG f (VGArray m1 n1 v1) (VGArray m2 n2 v2) =
  if m1 == m2 || n1 == n2
    then VGArray m1 n1 (VG.zipWith f v1 v2)
    else let !(m, n) = (min m1 m2, min n1 n2)
             getPx !k =
               let !ix = toIx n k
                   !px1 = VG.unsafeIndex v1 (fromIx n1 ix)
                   !px2 = VG.unsafeIndex v2 (fromIx n2 ix)
               in f px1 px2
         in VGArray m n $ VG.generate (m * n) getPx
{-# INLINE zipWithVG #-}


izipWithVG
  :: (VG.Vector v p1, VG.Vector v p2, VG.Vector v p)
  => ((Int, Int) -> p1 -> p2 -> p)
  -> VGArray v p1
  -> VGArray v p2
  -> VGArray v p
izipWithVG f (VGArray 1 1 v1) (VGArray m n v2) =
  let !px1 = VG.unsafeIndex v1 0
  in VGArray m n (VG.imap (\ !k !px2 -> f (toIx n k) px1 px2) v2)
izipWithVG f (VGArray m n v1) (VGArray 1 1 v2) =
  let !px2 = VG.unsafeIndex v2 0
  in VGArray m n (VG.imap (\ !k !px1 -> f (toIx n k) px1 px2) v1)
izipWithVG f (VGArray m1 n1 v1) (VGArray m2 n2 v2) =
  if m1 == m2 || n1 == n2
    then VGArray m1 n1 (VG.izipWith (\ !k !px1 !px2 -> f (toIx n1 k) px1 px2) v1 v2)
    else let !(m, n) = (min m1 m2, min n1 n2)
             getPx !k =
               let !ix = toIx n k
                   !px1 = VG.unsafeIndex v1 (fromIx n1 ix)
                   !px2 = VG.unsafeIndex v2 (fromIx n2 ix)
               in f ix px1 px2
         in VGArray m n $ VG.generate (m * n) getPx
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

indexVG :: VG.Vector v p => VGArray v p -> (Int, Int) -> p
indexVG (VGArray m n v) ix = indexV (m, n) v ix
{-# INLINE indexVG #-}

unsafeTraverseVG
  :: (VG.Vector v p1, VG.Vector v p)
  => VGArray v p1
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> (Int, Int) -> p)
  -> VGArray v p
unsafeTraverseVG (VGArray m n v) getNewDims getNewPx =
  makeArrayVG (getNewDims (m, n)) (getNewPx (unsafeIndexV n v))
{-# INLINE unsafeTraverseVG #-}


traverseVG
  :: (VG.Vector v p1, VG.Vector v p)
  => VGArray v p1
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> (Int, Int) -> p)
  -> VGArray v p
traverseVG (VGArray m n v) getNewDims getNewPx =
  makeArrayVG (getNewDims (m, n)) (getNewPx (indexV (m, n) v))
{-# INLINE traverseVG #-}


unsafeTraverse2VG
  :: (VG.Vector v p1, VG.Vector v p2, VG.Vector v p)
  => VGArray v p1
  -> VGArray v p2
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> ((Int, Int) -> p2) -> (Int, Int) -> p)
  -> VGArray v p
unsafeTraverse2VG (VGArray m1 n1 v1) (VGArray m2 n2 v2) getNewDims getNewPx =
  makeArrayVG
    (getNewDims (m1, n1) (m2, n2))
    (getNewPx (unsafeIndexV n1 v1) (unsafeIndexV n2 v2))
{-# INLINE unsafeTraverse2VG #-}


traverse2VG
  :: (VG.Vector v p1, VG.Vector v p2, VG.Vector v p)
  => VGArray v p1
  -> VGArray v p2
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> p1) -> ((Int, Int) -> p2) -> (Int, Int) -> p)
  -> VGArray v p
traverse2VG (VGArray m1 n1 v1) (VGArray m2 n2 v2) getNewDims getNewPx =
  makeArrayVG
    (getNewDims (m1, n1) (m2, n2))
    (getNewPx (indexV (m1, n1) v1) (indexV (m2, n2) v2))
{-# INLINE traverse2VG #-}


transposeVG :: (VG.Vector v Int, VG.Vector v p) =>
               VGArray v p -> VGArray v p
transposeVG (VGArray m n v) =
  VGArray n m $ VG.unsafeBackpermute v $ VG.generate (m * n) (fromIx n . swapIx . toIx m)
{-# INLINE transposeVG #-}

backpermuteWithCheckVG
  :: (VG.Vector v Int, VG.Vector v p)
  => ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> VGArray v p
  -> VGArray v p
backpermuteWithCheckVG checkIx !sz f (VGArray m' n' v) =
  let !(m, n) = checkDimsVG "backpermuteWithCheckVG" sz
  in VGArray m n $
     VG.unsafeBackpermute v $
     VG.generate (m * n) (fromIx n' . checkIx (m', n') . f . toIx n)
{-# INLINE backpermuteWithCheckVG #-}


unsafeBackpermuteVG
  :: (VG.Vector v Int, VG.Vector v p)
  => (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> VGArray v p
  -> VGArray v p
unsafeBackpermuteVG = backpermuteWithCheckVG (const id)
{-# INLINE unsafeBackpermuteVG #-}


backpermuteVG
  :: (VG.Vector v Int, VG.Vector v p)
  => (Int, Int)
  -> ((Int, Int) -> (Int, Int))
  -> VGArray v p
  -> VGArray v p
backpermuteVG =
  backpermuteWithCheckVG $ \ !sz !ix ->
    let err =
          errorVG
            "backpermuteVG"
            ("Index out of bounds <" ++ show sz ++ ">: " ++ show ix)
    in handleBorderIndex err sz id ix
{-# INLINE backpermuteVG #-}


fromListsVG :: VG.Vector v p => [[p]] -> VGArray v p
fromListsVG !ls =
  if all (== n) (fmap length ls)
    then VGArray m n . VG.fromList . concat $ ls
    else errorVG "fromListsVG" "Inner lists are of different lengths."
  where
    (m, n) =
      checkDimsVG "fromListsVG" (length ls, maybe 0 length $ listToMaybe ls)
{-# NOINLINE fromListsVG #-}

foldlVG :: VG.Vector v p =>
           (a -> p -> a) -> a -> VGArray v p -> a
foldlVG !f !px0 (VGArray _ _ v) = VG.foldl' f px0 v
{-# INLINE foldlVG #-}

foldrVG :: VG.Vector v p =>
           (p -> a -> a) -> a -> VGArray v p -> a
foldrVG !f !px0 (VGArray _ _ v) = VG.foldr' f px0 v
{-# INLINE foldrVG #-}


ifoldlVG :: VG.Vector v p =>
            (a -> (Int, Int) -> p -> a) -> a -> VGArray v p -> a
ifoldlVG !f !px0 (VGArray _ n v) =
  VG.ifoldl' (\ !acc !k -> f acc (toIx n k)) px0 v
{-# INLINE ifoldlVG #-}


toVectorVG :: (VG.Vector v p)
           => VGArray v p -> v p
toVectorVG (VGArray _ _ v) = v
{-# INLINE toVectorVG #-}

fromVectorVG :: VG.Vector v p =>
                (Int, Int) -> v p -> VGArray v p
fromVectorVG !(m, n) !v
  | m * n == VG.length v = VGArray m n v
  | otherwise =
    errorVG "fromVectorVG" $
    " array dimensions do not match the length of a vector: " ++
    show m ++ " * " ++ show n ++ " /= " ++ show (VG.length v)
{-# INLINE fromVectorVG #-}

multVG :: ( VG.Vector v Int
          , VG.Vector v p
          , Num p
          ) => VGArray v p -> VGArray v p -> VGArray v p
multVG (VGArray m1 n1 v1) img2 =
  if n1 /= m2
    then errorVG "multVG" $
         "Inner dimensions of arrays must agree, but received: " ++
         show (m1, n1) ++ " X " ++ show (m2, n2)
    else makeArrayVG (m1, n2) getPx
  where
    VGArray n2 m2 v2 = transposeVG img2
    getPx !(i, j) =
      VG.sum $
      VG.zipWith (*) (VG.slice (i * n1) n1 v1) (VG.slice (j * m2) m2 v2)
    {-# INLINE getPx #-}
{-# INLINE multVG #-}



------ Manifest


data MVGArray s v p = MVGArray !Int !Int (VG.Mutable v s p)


unsafeIndexVG :: VG.Vector v p => VGArray v p -> (Int, Int) -> p
unsafeIndexVG (VGArray _ n v) = VG.unsafeIndex v . fromIx n
{-# INLINE unsafeIndexVG #-}


makeArrayMVG :: (Monad m, VG.Vector v p) =>
                (Int, Int) -> ((Int, Int) -> m p) -> m (VGArray v p)
makeArrayMVG !sz !f =
  let !(m, n) = checkDimsVG "makeArrayMVG" sz in
    VGArray m n <$> VG.generateM (m * n) (f . toIx n)
{-# INLINE makeArrayMVG #-}

mapMVG
  :: (Monad m, VG.Vector v a, VG.Vector v p) =>
     (a -> m p) -> VGArray v a -> m (VGArray v p)
mapMVG f (VGArray m n v) = VGArray m n <$> VG.mapM f v
{-# INLINE mapMVG #-}

mapM_VG
  :: (Monad m, VG.Vector v a) => (a -> m b) -> VGArray v a -> m ()
mapM_VG f (VGArray _ _ v) = VG.mapM_ f v
{-# INLINE mapM_VG #-}

foldMVG
  :: (Monad m, VG.Vector v b) =>
     (a -> b -> m a) -> a -> VGArray v b -> m a
foldMVG f !a (VGArray _ _ v) = VG.foldM' f a v
{-# INLINE foldMVG #-}

foldM_VG
  :: (Monad m, VG.Vector v b) =>
     (a -> b -> m a) -> a -> VGArray v b -> m ()
foldM_VG f !a (VGArray _ _ v) = VG.foldM'_ f a v
{-# INLINE foldM_VG #-}


mdimsVG :: MVGArray t2 t1 t -> (Int, Int)
mdimsVG (MVGArray m n _) = (m, n)
{-# INLINE mdimsVG #-}

thawVG :: (VG.Mutable v1 ~ VG.Mutable v, PrimMonad f, VG.Vector v1 p) =>
     VGArray v1 p -> f (MVGArray (PrimState f) v p)
thawVG (VGArray m n v) = MVGArray m n <$> VG.thaw v
{-# INLINE thawVG #-}

freezeVG :: (VG.Mutable t ~ VG.Mutable v, PrimMonad f, VG.Vector v p) =>
     MVGArray (PrimState f) t p -> f (VGArray v p)
freezeVG (MVGArray m n mv) = VGArray m n <$> VG.freeze mv
{-# INLINE freezeVG #-}

newVG :: (PrimMonad f, MVG.MVector (VG.Mutable v) p) =>
     (Int, Int) -> f (MVGArray (PrimState f) v p)
newVG (m, n) = MVGArray m n <$> MVG.new (m*n)
{-# INLINE newVG #-}

readVG :: (PrimMonad m, MVG.MVector (VG.Mutable t) a) =>
     MVGArray (PrimState m) t a -> (Int, Int) -> m a
readVG (MVGArray _ n mv) !ix = MVG.read mv (fromIx n ix)
{-# INLINE readVG #-}

writeVG :: (PrimMonad m, MVG.MVector (VG.Mutable t) a) =>
     MVGArray (PrimState m) t a -> (Int, Int) -> a -> m ()
writeVG (MVGArray _ n mv) !ix !px = MVG.write mv (fromIx n ix) px
{-# INLINE writeVG #-}

swapVG :: (PrimMonad m, MVG.MVector (VG.Mutable v) p) =>
           MVGArray (PrimState m) v p -> (Int, Int) -> (Int, Int) -> m ()
swapVG (MVGArray _ n mv) !ix1 !ix2 = MVG.swap mv (fromIx n ix1) (fromIx n ix2)
{-# INLINE swapVG #-}


errorVG :: String -> String -> a
errorVG fName errMsg =
  error $ "Graphics.Image.Interface.Vector.Generic." ++ fName ++ ": " ++ errMsg



checkDimsVG :: String -> (Int, Int) -> (Int, Int)
checkDimsVG fName = checkDims ("Graphics.Image.Interface.Vector.Generic." ++ fName)
{-# INLINE checkDimsVG #-}
