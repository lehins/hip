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
  ( RImage
  , Strategy(..)
  , computeR
  , makeImageR
  , dimsR
  , scalarR
  , index00R
  , makeImageWindowedR
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
  , fromRepaArrayR
  , multR
  , ix2sh
  , sh2ix
  )where

import           Data.Array.Repa                     as R
import           Data.Array.Repa.Eval                as R (Elt (..), Target,
                                                           fromList,
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
import           Graphics.Image.Interface            (ColorSpace (..), Pixel,
                                                      checkDims, toIx)
import           Prelude                             as P

data Strategy
  = Parallel
  | Sequential

data RImage r p = RTImage !(R.Array r R.DIM2 p)
                | RDImage !(R.Array R.D R.DIM2 p)

sh2ix :: DIM2 -> (Int, Int)
sh2ix (Z :. i :. j) = (i, j)
{-# INLINE sh2ix #-}

ix2sh :: (Int, Int) -> DIM2
ix2sh !(i, j) = Z :. i :. j
{-# INLINE ix2sh #-}

errorR :: String -> String -> a
errorR fName errMsg =
  error $ "Graphics.Image.Interface.Repa.Generic." P.++ fName P.++ ": " P.++ errMsg

checkDimsR :: String -> (Int, Int) -> DIM2
checkDimsR fName = ix2sh . checkDims ("Graphics.Image.Interface.Repa.Generic." P.++ fName)
{-# INLINE checkDimsR #-}


dimsR :: Source r p => RImage r p -> (Int, Int)
dimsR (RTImage arr) = sh2ix $ R.extent arr
dimsR (RDImage arr) = sh2ix $ R.extent arr
{-# INLINE dimsR #-}

makeImageR :: (Int, Int) -> ((Int, Int) -> px) -> RImage t px
makeImageR sz f =
  RDImage $ R.fromFunction (checkDimsR "makeImageR" sz) (f . sh2ix)
{-# INLINE makeImageR #-}

makeImageWindowedR
  :: (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> ((Int, Int) -> p)
  -> ((Int, Int) -> p)
  -> RImage r p
makeImageWindowedR !sz !wIx !wSz getWindowPx getBorderPx =
  RDImage $ R.delay $ makeWindowed sh wIx wSz wArr bArr
  where
    wArr = R.fromFunction sh (getWindowPx . sh2ix)
    bArr = R.fromFunction sh (getBorderPx . sh2ix)
    !sh = checkDimsR "makeImageWindowedR" sz
{-# INLINE makeImageWindowedR #-}

makeWindowed
  :: (Source r1 p, Source r2 p)
  => DIM2 -- ^ Extent of array.
  -> (Int, Int) -- ^ Window starting index
  -> (Int, Int) -- ^ Window size.
  -> Array r1 DIM2 p -- ^ Array for internal elements.
  -> Array r2 DIM2 p -- ^ Array for border elements.
  -> Array (P r1 (P r2 (P r2 (P r2 (P r2 X))))) DIM2 p
makeWindowed sh@(Z :. m :. n) !(it, jt) !(wm, wn) arrWindow arrBorder =
  let inInternal (Z :. i :. j) = i >= it && i < ib && j >= jt && j < jb
      {-# INLINE inInternal #-}
      inBorder = not . inInternal
      {-# INLINE inBorder #-}
      !(ib, jb) = (wm + it, wn + jt)
  in APart sh (Range (Z :. it :. jt) (Z :. wm       :. wn      ) inInternal) arrWindow $
     APart sh (Range (Z :. 0  :. 0 ) (Z :. it       :. n       ) inBorder  ) arrBorder $
     APart sh (Range (Z :. it :. 0 ) (Z :. wm       :. jt      ) inBorder  ) arrBorder $
     APart sh (Range (Z :. it :. jb) (Z :. wm       :. (n - jb)) inBorder  ) arrBorder $
     APart sh (Range (Z :. ib :. 0 ) (Z :. (m - ib) :. n       ) inBorder  ) arrBorder $
     AUndefined sh
{-# INLINE makeWindowed #-}

fromRepaArrayR :: (Source r1 p) => R.Array r1 DIM2 p -> RImage r p
fromRepaArrayR = RDImage . R.delay
{-# INLINE fromRepaArrayR #-}


scalarR :: p -> RImage t p
scalarR = RDImage . fromFunction (Z :. 1 :. 1) . const
{-# INLINE scalarR #-}

index00R :: Source r e => RImage r e -> e
index00R (RTImage arr) = R.index arr (Z :. 0 :. 0)
index00R (RDImage arr) = R.index arr (Z :. 0 :. 0)
{-# INLINE index00R #-}

mapR :: Source r1 a => (a -> p) -> RImage r1 a -> RImage r p
mapR f (RTImage arr) = RDImage (R.map f arr)
mapR f (RDImage arr) = RDImage (R.map f arr)
{-# INLINE mapR #-}


imapR :: Source r2 b =>
         ((Int, Int) -> b -> p) -> RImage r2 b -> RImage r p
imapR f (RTImage arr) = RDImage (imapArr (f . sh2ix) arr)
imapR f (RDImage arr) = RDImage (imapArr (f . sh2ix) arr)
{-# INLINE imapR #-}


imapArr :: R.Source r2 b =>
           (DIM2 -> b -> c) -> R.Array r2 DIM2 b -> R.Array R.D DIM2 c
imapArr f !arr = R.traverse arr id (\ getPx !sh -> f sh (getPx sh))
{-# INLINE imapArr #-}


getDelayed :: Source r e => RImage r e -> Array D DIM2 e
getDelayed (RTImage arr) = delay arr
getDelayed (RDImage arr) = arr
{-# INLINE getDelayed #-}

zipWithR
  :: (Source r2 t, Source r1 e) =>
     (t -> e -> c) -> RImage r2 t -> RImage r1 e -> RImage r c
zipWithR f !img1 !img2 = zipWithArr f (getDelayed img1) (getDelayed img2)
{-# INLINE zipWithR #-}

zipWithArr
  :: (Source r1 a, Source r2 b)
  => (a -> b -> c) -> Array r1 DIM2 a -> Array r2 DIM2 b -> RImage r c
zipWithArr f arr1 arr2 =
  RDImage $
  case (extent arr1, extent arr2) of
    (Z :. 1 :. 1, _) -> R.map (f (unsafeIndex arr1 (Z :. 0 :. 0))) arr2
    (_, Z :. 1 :. 1) -> R.map (`f` (unsafeIndex arr2 (Z :. 0 :. 0))) arr1
    _                -> R.zipWith f arr1 arr2
{-# INLINE zipWithArr #-}

izipWithR
  :: (Source r2 a, Source r1 b) =>
     ((Int, Int) -> a -> b -> c)
     -> RImage r2 a -> RImage r1 b -> RImage r c
izipWithR f !img1 !img2 = izipWithArr f (getDelayed img1) (getDelayed img2)
{-# INLINE izipWithR #-}

izipWithArr
  :: (Source r1 a, Source r2 b)
  => ((Int, Int) -> a -> b -> c)
  -> Array r1 DIM2 a
  -> Array r2 DIM2 b
  -> RImage r c
izipWithArr f arr1 arr2 =
  RDImage $
  case (extent arr1, extent arr2) of
    (Z :. 1 :. 1, _) ->
      imapArr (\ !sh -> f (sh2ix sh) (unsafeIndex arr1 (Z :. 0 :. 0))) arr2
    (_, Z :. 1 :. 1) ->
      imapArr
        (\ !sh !px1 -> f (sh2ix sh) px1 (unsafeIndex arr2 (Z :. 0 :. 0))) arr1
    (Z :. m1 :. n1, Z :. m2 :. n2) -> traverse2 arr1 arr2 getNewDims getNewPx
      where getNewDims _ _ = Z :. min m1 m2 :. min n1 n2
            {-# INLINE getNewDims #-}
            getNewPx getPx1 getPx2 !sh = f (sh2ix sh) (getPx1 sh) (getPx2 sh)
            {-# INLINE getNewPx #-}
{-# INLINE izipWithArr #-}


traverseR
  :: Source r1 c
  => RImage r1 c
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> c) -> (Int, Int) -> p)
  -> RImage r p
traverseR img getNewDims getNewPx =
  RDImage $
  R.traverse
    (getDelayed img)
    (checkDimsR "traverseR" . getNewDims . sh2ix)
    (\ getPx -> getNewPx (getPx . ix2sh) . sh2ix)
{-# INLINE traverseR #-}

unsafeTraverseR
  :: Source r1 c
  => RImage r1 c
  -> ((Int, Int) -> (Int, Int))
  -> (((Int, Int) -> c) -> (Int, Int) -> p)
  -> RImage r p
unsafeTraverseR img getNewDims getNewPx =
  RDImage $
  R.unsafeTraverse
    (getDelayed img)
    (checkDimsR "traverseR" . getNewDims . sh2ix)
    (\ getPx -> getNewPx (getPx . ix2sh) . sh2ix)
{-# INLINE unsafeTraverseR #-}


traverse2R
  :: (Source r1 a, Source r2 b)
  => RImage r1 a
  -> RImage r2 b
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> a) -> ((Int, Int) -> b) -> (Int, Int) -> c)
  -> RImage r c
traverse2R img1 img2 getNewDims getNewPx =
  RDImage $
  R.traverse2
    (getDelayed img1)
    (getDelayed img2)
    (\ !sh1 !sh2 -> checkDimsR "traverse2R" (getNewDims (sh2ix sh1) (sh2ix sh2)))
    (\getPx1 getPx2 -> getNewPx (getPx1 . ix2sh) (getPx2 . ix2sh) . sh2ix)
{-# INLINE traverse2R #-}

unsafeTraverse2R
  :: (Source r1 a, Source r2 b)
  => RImage r1 a
  -> RImage r2 b
  -> ((Int, Int) -> (Int, Int) -> (Int, Int))
  -> (((Int, Int) -> a) -> ((Int, Int) -> b) -> (Int, Int) -> c)
  -> RImage r c
unsafeTraverse2R img1 img2 getNewDims getNewPx =
  RDImage $
  R.unsafeTraverse2
    (getDelayed img1)
    (getDelayed img2)
    (\ !sh1 !sh2 -> checkDimsR "traverse2R" (getNewDims (sh2ix sh1) (sh2ix sh2)))
    (\getPx1 getPx2 -> getNewPx (getPx1 . ix2sh) (getPx2 . ix2sh) . sh2ix)
{-# INLINE unsafeTraverse2R #-}


transposeR :: Source r1 p => RImage r1 p -> RImage r p
transposeR (RDImage arr) = RDImage (R.transpose arr)
transposeR (RTImage arr) = RDImage (R.transpose arr)
{-# INLINE transposeR #-}


backpermuteR
  :: R.Source r e
  => (Int, Int) -> ((Int, Int) -> (Int, Int)) -> RImage r e -> RImage r e
backpermuteR !newDims g !img =
  RDImage $ R.backpermute
    (ix2sh (checkDims "backpermuteR" newDims))
    (ix2sh . g . sh2ix)
    (getDelayed img)
{-# INLINE backpermuteR #-}




fromListsR :: Target r p => [[p]] -> RImage r p
fromListsR ls =
  if all (== n) (P.map length ls)
    then RTImage $ R.fromList (Z :. m :. n) . concat $ ls
    else error "fromListsRepa: Inner lists do not all have an equal length."
  where
    !(m, n) = checkDims "fromListsRepa" (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsR #-}



multR :: (Num p, Elt p, Unbox p, Source r2 p, Source r1 p, Source r p, Target r p,
           Target r2 p, Target r1 p) =>
         Strategy -> RImage r2 p -> RImage r1 p -> RImage r p
multR strategy (RTImage arr1) (RTImage arr2) =
  if n1 /= m2
    then errorR "multR" $
         "Inner dimensions of multiplied images must be the same, but received: " P.++
         show (m1, n1) P.++
         " X " P.++
         show (m2, n2)
    else computeR strategy $ RDImage $ fromFunction (Z :. m1 :. n2) getPx
  where
    (Z :. m1 :. n1) = R.extent arr1
    (Z :. m2 :. n2) = R.extent arr2
    getPx (Z :. i :. j) =
      R.sumAllS
        (R.slice arr1 (R.Any :. (i :: Int) :. R.All) R.*^
         R.slice arr2 (R.Any :. (j :: Int)))
    {-# INLINE getPx #-}
multR strategy img1 img2 =
  multR strategy (computeR strategy img1) (computeR strategy img2)
{-# INLINE multR #-}


computeR :: (Target r p, Source r p) => Strategy -> RImage r p -> RImage r p
computeR Parallel (RDImage arr) = arrManifest `deepSeqArray` RTImage arrManifest
  where arrManifest = suspendedComputeP arr
computeR Sequential (RDImage arr) = RTImage $ computeS arr
computeR _ img = img
{-# INLINE computeR #-}


foldR :: (Source r p, Target r p, Elt p, Unbox p)
       => Strategy -> (p -> p -> p) -> p -> RImage r p -> p
foldR Parallel f !px0 !img =
  let RTImage arr = computeR Parallel img
  in case foldAllP f px0 arr of
       Just e  -> e
       Nothing -> errorR "foldPR" "impossible happened."
foldR Sequential f !px0 img =
  let RTImage arr = computeR Sequential img
  in foldAllS f px0 arr
{-# INLINE foldR #-}


addIxArr
  :: Source r2 b =>
     Array r2 DIM2 b -> Array R.D DIM2 (Int, b)
addIxArr !arr = R.zipWith (,) arrIx arr
  where
    !sh = extent arr
    !arrIx = R.fromFunction (R.extent arr) (toIndex sh)
{-# INLINE addIxArr #-}


foldIxR :: (Source r b, Elt b, Unbox b) =>
           Strategy -> (b -> (Int, Int) -> b -> b) -> b -> RImage r b -> b
foldIxR strategy f !acc' !img =
  case strategy of
    Parallel ->
      case foldAllP accumWithIx (-1, acc') arr of
        Just (_, acc) -> acc
        Nothing       -> error $ "foldIxPR: impossible happened."
    Sequential -> snd $ R.foldAllS accumWithIx (-1, acc') arr
  where
    (Z :. _ :. n) = extent arr
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
        Strategy -> RImage r1 a -> RImage r a -> Bool
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



fromVectorUnboxedR :: Unbox e => (Int, Int) -> VU.Vector e -> RImage U e
fromVectorUnboxedR !sz = RTImage . fromUnboxed (ix2sh sz)
{-# INLINE fromVectorUnboxedR #-}



toVectorR :: (Target r2 e, Source r2 e) =>
             (R.Array r2 DIM2 e -> t) -> Strategy -> RImage r2 e -> t
toVectorR toVector Parallel (RDImage arr) =
 toVector $ mCompute "toVectorR" (computeP arr)
toVectorR toVector Sequential (RDImage arr) = toVector $ computeS arr
toVectorR toVector _ (RTImage arr) = toVector arr
{-# INLINE toVectorR #-}


toVectorUnboxedR :: Unbox e => Strategy -> RImage U e -> VU.Vector e
toVectorUnboxedR = toVectorR toUnboxed
{-# INLINE toVectorUnboxedR #-}

toVectorStorableR :: VS.Storable e => Strategy -> RImage F e -> VS.Vector e
toVectorStorableR = toVectorR toStorableR
{-# INLINE toVectorStorableR #-}



toStorableR :: VS.Storable a => Array F DIM2 a -> VS.Vector a
toStorableR arr = VS.unsafeFromForeignPtr0 (toForeignPtr arr) (m * n)
  where
    (Z :. m :. n) = R.extent arr
{-# INLINE toStorableR #-}


fromVectorStorableR
  :: VS.Storable px
  => (Int, Int) -> VS.Vector px -> RImage F px
fromVectorStorableR !(m, n) !v
  | sz == sz' = RTImage $ fromForeignPtr (ix2sh (m, n)) fp
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
