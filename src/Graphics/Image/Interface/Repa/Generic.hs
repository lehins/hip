{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 800
    {-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.Image.Interface.Repa.Generic
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa.Generic
  ( RArray
  , Strategy(..)
  , computeR
  , makeArrayR
  , dimsR
  , scalarR
  , unsafeIndexR
  , makeArrayWindowedR
  , mapR
  , imapR
  , eqR
  , zipWithR
  , izipWithR
  , unsafeTraverseR
  , traverseR
  , unsafeTraverse2R
  , traverse2R
  , transposeR
  , backpermuteR
  , fromListsR
  , foldR
  , foldIxR
  , toVectorUnboxedR
  , fromVectorUnboxedR
  , toVectorStorableR
  , fromVectorStorableR
  , toRepaArrayR
  , fromRepaArrayR
  , multR
  , sh2ix
  , ix2sh
  )where

import           Data.Array.Repa                     as R
import           Data.Array.Repa.Eval                as R (Elt (..), Target,
                                                           Load, LoadRange, fromList,
                                                           suspendedComputeP)
import           Data.Array.Repa.Operators.Traversal as R
import           Data.Array.Repa.Repr.ForeignPtr
import           Data.Array.Repa.Repr.Partitioned    (P, Range (..))
import           Data.Array.Repa.Repr.Unboxed        (Unbox)
import           Data.Array.Repa.Repr.Undefined      (X)
import           Data.Complex                        as C
import           Data.Maybe                          (listToMaybe)
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Unboxed                 as VU
import           Graphics.Image.ColorSpace.Binary    (Bit (..))
import           Graphics.Image.Interface            (ColorSpace (..), Pixel)
import           Graphics.Image.Utils                (checkDims, toIx, fromIx, (.:))
import           Prelude                             as P

data Strategy
  = Parallel
  | Sequential

data RArray r p = RTArray !(R.Array r (Int, Int) p)
                | RDArray !(R.Array R.D (Int, Int) p)

sh2ix :: DIM2 -> (Int, Int)
sh2ix (Z :. i :. j) = (i, j)
{-# INLINE sh2ix #-}

ix2sh :: (Int, Int) -> DIM2
ix2sh !(i, j) = Z :. i :. j
{-# INLINE ix2sh #-}

sh2ixArr :: Source r1 p => R.Array r1 DIM2 p -> R.Array D (Int, Int) p
sh2ixArr arr = R.backpermute (sh2ix (extent arr)) ix2sh arr
{-# INLINE sh2ixArr #-}


errorR :: String -> String -> a
errorR fName errMsg =
  error $ "Graphics.Array.Interface.Repa.Generic." P.++ fName P.++ ": " P.++ errMsg

checkDimsR :: String -> (Int, Int) -> (Int, Int)
checkDimsR fName = checkDims ("Graphics.Image.Interface.Repa.Generic." P.++ fName)
{-# INLINE checkDimsR #-}


dimsR :: Source r p => RArray r p -> (Int, Int)
dimsR (RTArray arr) = R.extent arr
dimsR (RDArray arr) = R.extent arr
{-# INLINE dimsR #-}

makeArrayR :: (Int, Int) -> ((Int, Int) -> px) -> RArray t px
makeArrayR sz f =
  RDArray $ R.fromFunction (checkDimsR "makeArrayR" sz) f
{-# INLINE makeArrayR #-}

makeArrayWindowedR
  :: forall r p . (Target r p, Source r p, Unbox p, Elt p)
  =>
  Strategy
  -> (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> RArray r p
makeArrayWindowedR !_strategy !sz !wIx !wSz getWindowPx getBorderPx =
  RDArray $ sh2ixArr $ arr
  where
    arr :: Array r DIM2 p
    arr = makeWindowed sh (ix2sh wIx) (ix2sh wSz) wArr bArr
    -- compute :: Load r1 DIM2 p => Array r1 DIM2 p -> Array r DIM2 p
    -- compute = case strategy of
    --             Parallel -> suspendedComputeP
    --             Sequential -> computeS
    wArr = R.fromFunction sh (getWindowPx . sh2ix)
    bArr = R.fromFunction sh (getBorderPx . sh2ix)
    !sh = ix2sh (checkDimsR "makeArrayWindowedR" sz)
{-# INLINE makeArrayWindowedR #-}

makeWindowed
  :: (Elt p, Unbox p, LoadRange r1 DIM2 p, LoadRange r2 DIM2 p, Source r1 p, Source r2 p, Target r p)
  => DIM2 -- ^ Extent of array.
  -> DIM2 -- ^ Window starting index
  -> DIM2 -- ^ Window size.
  -> Array r1 DIM2 p -- ^ Array for internal elements.
  -> Array r2 DIM2 p -- ^ Array for border elements.
  -> Array r DIM2 p
makeWindowed sh@(Z :. m :. n) (Z :. it :. jt) (Z :. wm :. wn) arrWindow arrBorder =
  let inInternal !(Z :. i :. j) = i >= it && i < ib && j >= jt && j < jb
      {-# INLINE inInternal #-}
      inBorder = not . inInternal
      {-# INLINE inBorder #-}
      !(ib, jb) = (wm + it, wn + jt)
  in suspendedComputeP $
     APart sh (Range (Z :. it :. jt) (Z :. wm :. wn) inInternal) arrWindow $
     APart sh (Range (Z :. 0 :. 0) (Z :. it :. n) inBorder) arrBorder $
     APart sh (Range (Z :. it :. 0) (Z :. wm :. jt) inBorder) arrBorder $
     APart sh (Range (Z :. it :. jb) (Z :. wm :. n - jb) inBorder) arrBorder $
     APart sh (Range (Z :. ib :. 0) (Z :. m - ib :. n) inBorder) arrBorder $
     AUndefined sh
{-# INLINE makeWindowed #-}

toRepaArrayR :: (Unbox e, Source r e) => RArray r e -> Array U DIM2 e
toRepaArrayR arr =
  R.computeUnboxedS
    (R.fromFunction (ix2sh (dimsR arr)) (R.index (getDelayed arr) . sh2ix))
{-# INLINE toRepaArrayR #-}

fromRepaArrayR :: Source r1 p => R.Array r1 DIM2 p -> RArray r p
fromRepaArrayR arr = RDArray (R.backpermute (sh2ix (extent arr)) ix2sh arr)
{-# INLINE fromRepaArrayR #-}


scalarR :: p -> RArray t p
scalarR = RDArray . fromFunction (1, 1) . const
{-# INLINE scalarR #-}

unsafeIndexR :: Source r e => RArray r e -> (Int, Int) -> e
unsafeIndexR (RTArray arr) = R.unsafeIndex arr
unsafeIndexR (RDArray arr) = R.unsafeIndex arr
{-# INLINE unsafeIndexR #-}

mapR :: Source r1 a => (a -> p) -> RArray r1 a -> RArray r p
mapR f (RTArray arr) = RDArray (R.map f arr)
mapR f (RDArray arr) = RDArray (R.map f arr)
{-# INLINE mapR #-}


imapR :: Source r2 b =>
         ((Int, Int) -> b -> p) -> RArray r2 b -> RArray r p
imapR f (RTArray arr) = RDArray (imapArr f arr)
imapR f (RDArray arr) = RDArray (imapArr f arr)
{-# INLINE imapR #-}


imapArr :: R.Source r2 b =>
           ((Int, Int) -> b -> c) -> R.Array r2 (Int, Int) b -> R.Array R.D (Int, Int) c
imapArr f !arr = R.traverse arr id (\ getPx !sh -> f sh (getPx sh))
{-# INLINE imapArr #-}


getDelayed :: Source r e => RArray r e -> Array D (Int, Int) e
getDelayed (RTArray arr) = delay arr
getDelayed (RDArray arr) = arr
{-# INLINE getDelayed #-}

zipWithR
  :: (Source r2 t, Source r1 e) =>
     (t -> e -> c) -> RArray r2 t -> RArray r1 e -> RArray r c
zipWithR f !img1 !img2 = zipWithArr f (getDelayed img1) (getDelayed img2)
{-# INLINE zipWithR #-}

zipWithArr
  :: (Source r1 a, Source r2 b)
  => (a -> b -> c) -> Array r1 (Int, Int) a -> Array r2 (Int, Int) b -> RArray r c
zipWithArr f arr1 arr2 =
  RDArray $
  case (extent arr1, extent arr2) of
    ((1, 1), _) -> R.map (f (unsafeIndex arr1 (0, 0))) arr2
    (_, (1, 1)) -> R.map (`f` (unsafeIndex arr2 (0, 0))) arr1
    _           -> R.zipWith f arr1 arr2
{-# INLINE zipWithArr #-}

izipWithR
  :: (Source r2 a, Source r1 b) =>
     ((Int, Int) -> a -> b -> c)
     -> RArray r2 a -> RArray r1 b -> RArray r c
izipWithR f !img1 !img2 = izipWithArr f (getDelayed img1) (getDelayed img2)
{-# INLINE izipWithR #-}

izipWithArr
  :: (Source r1 a, Source r2 b)
  => ((Int, Int) -> a -> b -> c)
  -> Array r1 (Int, Int) a
  -> Array r2 (Int, Int) b
  -> RArray r c
izipWithArr f arr1 arr2 =
  RDArray $
  case (extent arr1, extent arr2) of
    ((1, 1), _) ->
      imapArr (\ !sh -> f sh (unsafeIndex arr1 (0, 0))) arr2
    (_, (1,1)) ->
      imapArr
        (\ !sh !px1 -> f sh px1 (unsafeIndex arr2 (0, 0))) arr1
    _          -> traverse2 arr1 arr2 intersectDim getNewPx
      where getNewPx getPx1 getPx2 !sh = f sh (getPx1 sh) (getPx2 sh)
            {-# INLINE getNewPx #-}
{-# INLINE izipWithArr #-}


traverseR
  :: Source r1 c
  => RArray r1 c
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> c) -> (Int, Int) -> p)
  -> RArray r p
traverseR img getNewDims =
  RDArray . R.traverse (getDelayed img) (checkDimsR "traverseR" . getNewDims)
{-# INLINE traverseR #-}

unsafeTraverseR
  :: Source r1 c
  => RArray r1 c
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> c) -> (Int, Int) -> p)
  -> RArray r p
unsafeTraverseR img getNewDims =
  RDArray . R.unsafeTraverse (getDelayed img) getNewDims
{-# INLINE unsafeTraverseR #-}


traverse2R
  :: (Source r1 a, Source r2 b)
  => RArray r1 a
  -> RArray r2 b
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> a) -> ((Int, Int) -> b) -> (Int, Int) -> c)
  -> RArray r c
traverse2R img1 img2 getNewDims =
  RDArray .
  R.traverse2
    (getDelayed img1)
    (getDelayed img2)
    (checkDimsR "traverse2R" .: getNewDims)
{-# INLINE traverse2R #-}

unsafeTraverse2R
  :: (Source r1 a, Source r2 b)
  => RArray r1 a
  -> RArray r2 b
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> a) -> ((Int, Int) -> b) -> (Int, Int) -> c)
  -> RArray r c
unsafeTraverse2R img1 img2 getNewDims =
  RDArray . R.unsafeTraverse2 (getDelayed img1) (getDelayed img2) getNewDims
{-# INLINE unsafeTraverse2R #-}

transposeR' = undefined

transposeR :: Source r1 p => RArray r1 p -> RArray r p
transposeR (RDArray arr) = RDArray (transposeR' arr)
transposeR (RTArray arr) = RDArray (transposeR' arr)
{-# INLINE transposeR #-}


backpermuteR
  :: R.Source r e
  => (Int, Int) -> ((Int, Int) -> (Int, Int)) -> RArray r e -> RArray r e
backpermuteR !newDims g !img =
  RDArray $ R.backpermute (checkDims "backpermuteR" newDims) g (getDelayed img)
{-# INLINE backpermuteR #-}




fromListsR :: Target r p => [[p]] -> RArray r p
fromListsR ls =
  if all (== n) (P.map length ls)
    then RTArray $ R.fromList (m, n) . concat $ ls
    else error "fromListsRepa: Inner lists do not all have an equal length."
  where
    !(m, n) = checkDims "fromListsRepa" (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsR #-}

multR = undefined

-- multR :: (Num p, Elt p, Unbox p, Source r2 p, Source r1 p, Source r p, Target r p,
--            Target r2 p, Target r1 p) =>
--          Strategy -> RArray r2 p -> RArray r1 p -> RArray r p
-- multR strategy (RTArray arr1) (RTArray arr2) =
--   if n1 /= m2
--     then errorR "multR" $
--          "Inner dimensions of multiplied images must be the same, but received: " P.++
--          show (m1, n1) P.++
--          " X " P.++
--          show (m2, n2)
--     else computeR strategy $ RDArray $ fromFunction (m1, n2) getPx
--   where
--     (m1, n1) = R.extent arr1
--     (m2, n2) = R.extent arr2
--     getPx (i, j) =
--       R.sumAllS
--         (R.slice arr1 (R.Any :. (i :: Int) :. R.All) R.*^
--          R.slice arr2 (R.Any :. (j :: Int)))
--     {-# INLINE getPx #-}
-- multR strategy img1 img2 =
--   multR strategy (computeR strategy img1) (computeR strategy img2)
-- {-# INLINE multR #-}


computeR :: (Target r p, Source r p) => Strategy -> RArray r p -> RArray r p
computeR Parallel (RDArray arr) = arrManifest `deepSeqArray` RTArray arrManifest
  where arrManifest = suspendedComputeP arr
computeR Sequential (RDArray arr) = RTArray $ computeS arr
computeR _ img = img
{-# INLINE computeR #-}


foldR :: (Source r p, Target r p, Elt p, Unbox p)
       => Strategy -> (p -> p -> p) -> p -> RArray r p -> p
foldR Parallel f !px0 !img =
  let RTArray arr = computeR Parallel img
  in case foldAllP f px0 arr of
       Just e  -> e
       Nothing -> errorR "foldPR" "impossible happened."
foldR Sequential f !px0 img =
  let RTArray arr = computeR Sequential img
  in foldAllS f px0 arr
{-# INLINE foldR #-}


addIxArr
  :: Source r2 b =>
     Array r2 (Int, Int) b -> Array R.D (Int, Int) (Int, b)
addIxArr !arr = R.zipWith (,) arrIx arr
  where
    !sh = extent arr
    !arrIx = R.fromFunction (R.extent arr) (toIndex sh)
{-# INLINE addIxArr #-}


foldIxR :: (Source r b, Elt b, Unbox b) =>
           Strategy -> (b -> (Int, Int) -> b -> b) -> b -> RArray r b -> b
foldIxR strategy f !acc' !img =
  case strategy of
    Parallel ->
      case foldAllP accumWithIx (-1, acc') arr of
        Just (_, acc) -> acc
        Nothing       -> error $ "foldIxPR: impossible happened."
    Sequential -> snd $ R.foldAllS accumWithIx (-1, acc') arr
  where
    (_, n) = extent arr
    !arr = addIxArr (getDelayed img)
    accumWithIx !(-1, acc) !(k, px) = (-1, f acc (toIx n k) px)
    accumWithIx !(k, px) (-1, acc) = (-1, f acc (toIx n k) px)
    accumWithIx (acc1Ix, _) (acc2Ix, _) =
      errorR "accumWithx" $
      "Impossible happened. Received: " P.++ show acc1Ix P.++ " " P.++
      show acc2Ix
    {-# INLINE accumWithIx #-}
{-# INLINE foldIxR #-}


eqR :: (Eq a, Source r1 a, Source r a) =>
        Strategy -> RArray r1 a -> RArray r a -> Bool
eqR strategy !img1 !img2 =
  case strategy of
    Parallel ->
      let eqArr = R.zipWith (==) (getDelayed img1) (getDelayed img2)
      in dimsR img1 == dimsR img2 && mCompute "eqR" (foldAllP (&&) True eqArr)
    Sequential -> R.equalsS (getDelayed img1) (getDelayed img2)
{-# INLINE eqR #-}

mCompute :: String -> Maybe t -> t
mCompute _ (Just res)  = res
mCompute fName Nothing = errorR fName "impossible happened"
{-# INLINE mCompute #-}



fromVectorUnboxedR :: Unbox e => (Int, Int) -> VU.Vector e -> RArray U e
fromVectorUnboxedR !sz = RTArray . fromUnboxed sz
{-# INLINE fromVectorUnboxedR #-}



toVectorR :: (Target r2 e, Source r2 e) =>
             (R.Array r2 (Int, Int) e -> t) -> Strategy -> RArray r2 e -> t
toVectorR toVector Parallel (RDArray arr) =
 toVector $ mCompute "toVectorR" (computeP arr)
toVectorR toVector Sequential (RDArray arr) = toVector $ computeS arr
toVectorR toVector _ (RTArray arr) = toVector arr
{-# INLINE toVectorR #-}


toVectorUnboxedR :: Unbox e => Strategy -> RArray U e -> VU.Vector e
toVectorUnboxedR = toVectorR toUnboxed
{-# INLINE toVectorUnboxedR #-}

toVectorStorableR :: VS.Storable e => Strategy -> RArray F e -> VS.Vector e
toVectorStorableR = toVectorR toStorableR
{-# INLINE toVectorStorableR #-}



toStorableR :: VS.Storable a => Array F (Int, Int) a -> VS.Vector a
toStorableR arr = VS.unsafeFromForeignPtr0 (toForeignPtr arr) (size (R.extent arr))
{-# INLINE toStorableR #-}


fromVectorStorableR
  :: VS.Storable px
  => (Int, Int) -> VS.Vector px -> RArray F px
fromVectorStorableR !(m, n) !v
  | sz == sz' = RTArray $ fromForeignPtr (m, n) fp
  | otherwise =
    errorR "fromVectorStorableR" $
    "(impossible) Vector size mismatch: " P.++ show sz P.++ " vs " P.++ show sz'
  where
    !(fp, sz) = VS.unsafeToForeignPtr0 v
    !sz' = m * n
{-# INLINE fromVectorStorableR #-}




instance (Num e, R.Elt e) => Elt (C.Complex e) where
  touch (r :+ i) = R.touch r >> R.touch i
  {-# INLINE touch #-}

  zero     = 0 :+ 0
  {-# INLINE zero #-}

  one      = 1 :+ 0
  {-# INLINE one #-}



instance Elt Bit where
  touch (Bit w) = R.touch w
  {-# INLINE touch #-}

  zero     = 0
  {-# INLINE zero #-}

  one      = 1
  {-# INLINE one #-}


instance (ColorSpace cs e, Elt e, Num (Pixel cs e)) => Elt (Pixel cs e) where
  touch = foldlPx (\ a !e -> a >> touch e) (return ())
  {-# INLINE touch #-}

  zero     = 0
  {-# INLINE zero #-}

  one      = 1
  {-# INLINE one #-}


instance Shape (Int, Int) where

  rank _ = 2
  {-# INLINE [1] rank #-}

  zeroDim = (0, 0)
  {-# INLINE [1] zeroDim #-}

  unitDim = (1, 1)
  {-# INLINE [1] unitDim #-}

  intersectDim !(i1, j1) !(i2, j2) = (min i1 i2, min j1 j2)
  {-# INLINE [1] intersectDim #-}

  addDim !(i1, j1) !(i2, j2) = (i1 + i2, j1 + j2)
  {-# INLINE [1] addDim #-}

  size !(i, j) = i * j
  {-# INLINE [1] size #-}

  sizeIsValid !(i, j) = i > 0 && j <= (maxBound `div` i)
  {-# INLINE [1] sizeIsValid #-}

  toIndex !(_, n) = fromIx n
  {-# INLINE [1] toIndex #-}

  fromIndex !(_, n) = toIx n
  {-# INLINE [1] fromIndex #-}

  inShapeRange !(m, n) !(i1, j1) !(i2, j2) = (j2 >= n) && (j2 < j1) && (i2 >= m) && (i2 < i1)
  {-# INLINE [1] inShapeRange #-}

  listOfShape !(i, j) = [j, i]
  {-# NOINLINE listOfShape #-}

  shapeOfList [j, i] = (i, j)
  shapeOfList ls = errorR "Shape.shapeOfList" $ "Unexpected pattern: " P.++ show ls
  {-# NOINLINE shapeOfList #-}

  deepSeq (i, j) = seq i (seq j)
  {-# INLINE deepSeq #-}



instance Shape Int where

  rank _ = 1
  {-# INLINE [1] rank #-}

  zeroDim = 0
  {-# INLINE [1] zeroDim #-}

  unitDim = 1
  {-# INLINE [1] unitDim #-}

  intersectDim !k1 !k2 = min k1 k2
  {-# INLINE [1] intersectDim #-}

  addDim !k1 !k2 = k1 + k2
  {-# INLINE [1] addDim #-}

  size !k = k
  {-# INLINE [1] size #-}

  sizeIsValid !k = k >= 0
  {-# INLINE [1] sizeIsValid #-}

  toIndex _ = id
  {-# INLINE [1] toIndex #-}

  fromIndex _ = id
  {-# INLINE [1] fromIndex #-}

  inShapeRange !z !k1 !k2 = (z <= k2) && (k2 < k1)
  {-# INLINE [1] inShapeRange #-}

  listOfShape !k = [k]
  {-# NOINLINE listOfShape #-}

  shapeOfList [k] = k
  shapeOfList ls = errorR "Shape.shapeOfList" $ "Unexpected pattern: " P.++ show ls
  {-# NOINLINE shapeOfList #-}

  deepSeq = seq
  {-# INLINE deepSeq #-}
