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
  RD(..), RP(..), RS(..), computeP, computeS, delay,
  fromRepaArray, toRepaArray
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
import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V ((!))

import Data.Typeable (Typeable)
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R (Elt(..), suspendedComputeP)


-- | Repa 'D'elayed Array representation, which allows for fusion of computation.
data RD = RD

-- | Repa 'U'nboxed Array representation, which is computed in parallel.
data RP = RP

-- | Repa 'U'nboxed Array representation, which is computed sequentially. 
data RS = RS

instance Show RD where
  show _ = "RepaDelayed"

instance Show RP where
  show _ = "RepaParallel"
  
instance Show RS where
  show _ = "RepaSequential"

instance Elt RD cs e => Array RD cs e where
  type Elt RD cs e = (ColorSpace cs, Num e, Typeable e, R.Elt e, Unbox e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
                     
  data Image RD cs e = RScalar !(Pixel cs e)
                     | RUImage !(R.Array R.U R.DIM2 (Pixel cs e))
                     | RDImage !(R.Array R.D R.DIM2 (Pixel cs e))

  dims (RScalar _                          ) = (1, 1)
  dims (RUImage (R.extent -> (Z :. m :. n))) = (m, n)
  dims (RDImage (R.extent -> (Z :. m :. n))) = (m, n)
  {-# INLINE dims #-}

  singleton = RScalar
  {-# INLINE singleton #-}

  makeImage !(checkDims "RD.makeImage" -> (m, n)) !f =
    RDImage $ R.fromFunction (Z :. m :. n) (f . shT2)
  {-# INLINE makeImage #-}

  map f (RScalar px)        = RScalar (f px)
  map f (getDelayed -> arr) = RDImage (R.map f arr)
  {-# INLINE map #-}

  imap f (RScalar px)  = RScalar (f (0, 0) px)
  imap f (getDelayed -> arr) = RDImage (R.zipWith f (R.fromFunction (R.extent arr) shT2) arr)
  {-# INLINE imap #-}
    
  zipWith f (RScalar px1)        (RScalar px2)        = RScalar (f px1 px2)
  zipWith f (RScalar px1)        (getDelayed -> arr2) = RDImage (R.map (f   px1) arr2)
  zipWith f (getDelayed -> arr1) (RScalar px2)        = RDImage (R.map (`f` px2) arr1)
  zipWith f (getDelayed -> arr1) (getDelayed -> arr2) = RDImage (R.zipWith f arr1 arr2)
  {-# INLINE zipWith #-}

  izipWith f (RScalar px1)        (RScalar px2)        = RScalar (f (0, 0) px1 px2)
  izipWith f (RScalar px1)        !img2                = imap (`f` px1) img2
  izipWith f !img1                (RScalar px2)        = imap (\ !ix !px -> f ix px px2) img1
  izipWith f (getDelayed -> arr1) (getDelayed -> arr2) =
    RDImage (R.traverse2 arr1 arr2 const getNewPx) where
      getNewPx !getPx1 !getPx2 !sh = f (shT2 sh) (getPx1 sh) (getPx2 sh)
      {-# INLINE getNewPx #-}
  {-# INLINE izipWith #-}

  traverse (getDelayed -> arr) newDims newPx =
    RDImage $ R.traverse arr (tSh2 . checkDims "RD.traverse" . newDims . shT2) newPixel
    where
      newPixel getPx = newPx (getPx . tSh2) . shT2
  {-# INLINE traverse #-}

  traverse2 (getDelayed -> arr1) (getDelayed -> arr2) newDims newPx =
    RDImage $ R.traverse2 arr1 arr2 getNewDims getNewPx
    where getNewPx getPx1 getPx2 = newPx (getPx1 . tSh2) (getPx2 . tSh2) . shT2
          {-# INLINE getNewPx #-}
          getNewDims !dims1 !dims2 =
            tSh2 . checkDims "RD.traverse2" $ newDims (shT2 dims1) (shT2 dims2)
          {-# INLINE getNewDims #-}
  {-# INLINE traverse2 #-}

  transpose (RDImage arr) = RDImage (R.transpose arr)
  transpose (RUImage arr) = RDImage (R.transpose arr)
  transpose !img          = img
  {-# INLINE transpose #-}

  backpermute !(tSh2 . checkDims "RD.backpermute" -> sh) _ (RScalar px) =
    RDImage $ R.fromFunction sh (const px)
  backpermute !(tSh2 . checkDims "RD.backpermute" -> sh) g (getDelayed -> arr) =
    RDImage (R.backpermute sh (tSh2 . g . shT2) arr)
  {-# INLINE backpermute #-}

  fromLists !ls = if all (==n) (P.map length ls)
                  then RUImage . R.fromListUnboxed (Z :. m :. n) . concat $ ls
                  else error "RD.fromLists: Inner lists do not all have an equal length."
    where
      !(m, n) = checkDims "RD.fromLists" (length ls, length $ head ls)
  {-# INLINE fromLists #-}
  


instance Elt RS cs e => Array RS cs e where
  type Elt RS cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e, Typeable e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RS cs e = RSImage !(Image RD cs e)

  dims (RSImage img) = dims img
  {-# INLINE dims #-}

  makeImage !(checkDims "RS.makeImage" -> ix) !f = computeS (makeImage ix f :: Image RD cs e)
  {-# INLINE makeImage #-}

  singleton = RSImage . singleton
  {-# INLINE singleton #-}

  map !f (RSImage img) = computeS $ map f img
  {-# INLINE map #-}

  imap !f (RSImage img) = computeS $ imap f img
  {-# INLINE imap #-}

  zipWith !f (RSImage img1) (RSImage img2) = computeS $ zipWith f img1 img2
  {-# INLINE zipWith #-}

  izipWith !f (RSImage img1) (RSImage img2) = computeS $ izipWith f img1 img2
  {-# INLINE izipWith #-}

  traverse (RSImage img) newDims = computeS . traverse img newDims 
  {-# INLINE traverse #-}

  traverse2 (RSImage img1) (RSImage img2) newDims = computeS . traverse2 img1 img2 newDims 
  {-# INLINE traverse2 #-}

  transpose (RSImage img) = computeS $ transpose img
  {-# INLINE transpose #-}
  
  backpermute !f !g (RSImage img) = computeS $ backpermute f g img
  {-# INLINE backpermute #-}

  fromLists = RSImage . fromLists
  {-# INLINE fromLists #-}



instance Elt RP cs e => Array RP cs e where
  type Elt RP cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e, Typeable e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RP cs e = RPImage !(Image RD cs e)

  dims (RPImage img) = dims img
  {-# INLINE dims #-}

  makeImage !(checkDims "RP.makeImage" -> ix) !f = suspendedComputeP $ makeImage ix f
  {-# INLINE makeImage #-}

  singleton = RPImage . singleton
  {-# INLINE singleton #-}

  map !f (RPImage img) = suspendedComputeP $ map f img
  {-# INLINE map #-}

  imap !f (RPImage img) = suspendedComputeP $ imap f img
  {-# INLINE imap #-}

  zipWith !f (RPImage img1) (RPImage img2) = suspendedComputeP $ zipWith f img1 img2
  {-# INLINE zipWith #-}

  izipWith !f (RPImage img1) (RPImage img2) = suspendedComputeP $ izipWith f img1 img2
  {-# INLINE izipWith #-}

  traverse (RPImage img) newDims = suspendedComputeP . traverse img newDims 
  {-# INLINE traverse #-}

  traverse2 (RPImage img1) (RPImage img2) newDims =
    suspendedComputeP . traverse2 img1 img2 newDims 
  {-# INLINE traverse2 #-}

  transpose (RPImage img) = suspendedComputeP $ transpose img
  {-# INLINE transpose #-}
  
  backpermute !f !g (RPImage img) = suspendedComputeP $ backpermute f g img
  {-# INLINE backpermute #-}

  fromLists = RPImage . fromLists
  {-# INLINE fromLists #-}


  
instance Array RS cs e => ManifestArray RS cs e where

  unsafeIndex (RSImage (RUImage arr)) (i, j) = R.index arr (Z :. i :. j)
  unsafeIndex (RSImage (RScalar px))  _      = px
  unsafeIndex _ _ = _errorCompute "ManifestArray RS cs e :: unsafeIndex"
  {-# INLINE unsafeIndex #-}

  deepSeqImage (RSImage (RUImage arr)) = R.deepSeqArray arr
  deepSeqImage (RSImage (RScalar px))  = seq px
  deepSeqImage _ = _errorCompute "ManifestArray RS cs e :: deepSeqImage"
  {-# INLINE deepSeqImage #-}

  (|*|) i1@(RSImage img1) i2@(RSImage img2) =
    i1 `deepSeqImage` i2 `deepSeqImage` computeS (mult img1 img2)
  {-# INLINE (|*|) #-}

  fold !f !px0 (RSImage (RUImage arr)) = R.foldAllS f px0 arr
  fold !f !px0 (RSImage (RScalar px))  = f px0 px
  fold _  _  _ = _errorCompute "ManifestArray RS cs e :: fold"
  {-# INLINE fold #-}

  eq (RSImage (RUImage arr1)) (RSImage (RUImage arr2)) = R.equalsS arr1 arr2
  eq (RSImage (RScalar arr1)) (RSImage (RScalar arr2)) = arr1 == arr2
  eq (RSImage (RUImage arr1)) (RSImage (RScalar arr2))
    | R.extent arr1 == (Z :. 1 :. 1) = R.index arr1 (Z :. 0 :. 0) == arr2
    | otherwise = False
  eq img1@(RSImage (RScalar _)) img2@(RSImage (RUImage _)) = img2 `eq` img1
  eq (RSImage (RDImage _)) _ = _errorCompute "ManifestArray RS cs e :: eq"
  eq _ (RSImage (RDImage _)) = _errorCompute "ManifestArray RS cs e :: eq"
  {-# INLINE eq #-}


instance Array RP cs e => ManifestArray RP cs e where

  unsafeIndex (RPImage (RUImage arr)) (i, j) = R.unsafeIndex arr (Z :. i :. j)
  unsafeIndex (RPImage (RScalar px))  _      = px
  unsafeIndex _ _ = _errorCompute "ManifestArray RP cs e :: unsafeIndex"
  {-# INLINE unsafeIndex #-}

  deepSeqImage (RPImage (RUImage arr)) = R.deepSeqArray arr
  deepSeqImage (RPImage (RScalar px))  = seq px
  deepSeqImage _ = _errorCompute "ManifestArray RP cs e :: deepSeqImage"
  {-# INLINE deepSeqImage #-}

  (|*|) i1@(RPImage img1) i2@(RPImage img2) =
    i1 `deepSeqImage` i2 `deepSeqImage` suspendedComputeP (mult img1 img2)
  {-# INLINE (|*|) #-}

  fold !f !px0 (RPImage (RUImage arr)) = head . R.foldAllP f px0 $ arr
  fold !f !px0 (RPImage (RScalar px))  = f px0 px
  fold _  _  _ = _errorCompute "ManifestArray RP cs e :: fold"
  {-# INLINE fold #-}

  eq (RPImage (RUImage arr1)) (RPImage (RUImage arr2)) = head $ R.equalsP arr1 arr2
  eq (RPImage (RScalar arr1)) (RPImage (RScalar arr2)) = arr1 == arr2
  eq (RPImage (RUImage arr1)) (RPImage (RScalar arr2))
    | R.extent arr1 == (Z :. 1 :. 1) = R.index arr1 (Z :. 0 :. 0) == arr2
    | otherwise = False
  eq img1@(RPImage (RScalar _)) img2@(RPImage (RUImage _)) = img2 `eq` img1
  eq (RPImage (RDImage _)) _ = _errorCompute "ManifestArray RP cs e :: eq"
  eq _ (RPImage (RDImage _)) = _errorCompute "ManifestArray RP cs e :: eq"
  {-# INLINE eq #-}

  
instance ManifestArray RS cs e => SequentialArray RS cs e where

  foldl !f !a = foldl f a . exchange VU
  {-# INLINE foldl #-}

  foldr !f !a = foldr f a . exchange VU
  {-# INLINE foldr #-}

  makeImageM !(checkDims "RS.makeImageM" -> ix) !f = fmap (exchangeFrom VU RS) (makeImageM ix f)

  mapM !f img = fmap (exchange RS) (mapM f (exchange VU img))
  {-# INLINE mapM #-}

  mapM_ !f img = mapM_ f (exchange VU img)
  {-# INLINE mapM_ #-}

  foldM !f !a = foldM f a . exchange VU
  {-# INLINE foldM #-}

  foldM_ !f !a = foldM_ f a . exchange VU
  {-# INLINE foldM_ #-}


instance ManifestArray RS cs e => MutableArray RS cs e where

  data MImage st RS cs e = MRSImage !(MImage st VU cs e)

  mdims (MRSImage (mdims -> sz)) = sz
  {-# INLINE mdims #-}

  thaw img = fmap MRSImage (thaw (exchange VU img))
  {-# INLINE thaw #-}

  freeze (MRSImage mimg) = fmap (exchange RS) (freeze mimg)
  {-# INLINE freeze #-}

  new sz = fmap MRSImage (new sz)
  {-# INLINE new #-}

  read (MRSImage mimg) = read mimg
  {-# INLINE read #-}
  
  write (MRSImage mimg) = write mimg
  {-# INLINE write #-}

  swap (MRSImage mimg) = swap mimg
  {-# INLINE swap #-}


-- | O(1) - Delays manifest array.
instance Exchangable RS RD where

  exchange _ (RSImage img) = img
  {-# INLINE exchange #-}


-- | O(1) - Delays manifest array.
instance Exchangable RP RD where
  
  exchange _ (RPImage img) = img
  {-# INLINE exchange #-}

-- | Computes delayed array sequentially.
instance Exchangable RD RS where    

  exchange _ (RDImage arr) = RSImage . RUImage . R.computeS $ arr
  exchange _ img           = RSImage img
  {-# INLINE exchange #-}


-- | O(1) - Changes computation strategy.
instance Exchangable RP RS where
  
  exchange _ (RPImage img) = RSImage img
  {-# INLINE exchange #-}


-- | Computes delayed array in parallel.
instance Exchangable RD RP where
  
  exchange _ (RDImage arr) = RPImage . RUImage . R.suspendedComputeP $ arr
  exchange _ img           = RPImage img
  {-# INLINE exchange #-}


-- | O(1) - Changes computation strategy.
instance Exchangable RS RP where
  
  exchange _ (RSImage img) = RPImage img
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable VU RD where
  exchange _ = delay . exchange RS
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable VU RS where
  exchange _ img@(dims -> (1, 1)) = singleton (toUnboxedVector img V.! 0)
  exchange _ img = RSImage . RUImage . R.fromUnboxed (tSh2 $ dims img) . toUnboxedVector $ img
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable VU RP where
  exchange _ img@(dims -> (1, 1)) = singleton (toUnboxedVector img V.! 0)
  exchange _ img = RPImage . RUImage . R.fromUnboxed (tSh2 $ dims img) . toUnboxedVector $ img
  {-# INLINE exchange #-}


-- | O(1) - Changes to Vector representation.
instance Exchangable RS VU where
  exchange _ img@(RSImage (RUImage arr)) = fromUnboxedVector (dims img) (R.toUnboxed arr)
  exchange _ (RSImage (RScalar px)) = singleton px
  exchange _ _                     = _errorCompute "Exchangable RS VU :: unsafeIndex"
  {-# INLINE exchange #-}


-- | O(1) - Changes to Vector representation.
instance Exchangable RP VU where
  exchange _ img@(RPImage (RUImage arr)) = fromUnboxedVector (dims img) (R.toUnboxed arr)
  exchange _ (RPImage (RScalar px)) = singleton px
  exchange _ _                     = _errorCompute "Exchangable RP VU :: unsafeIndex"
  {-# INLINE exchange #-}

-- | Computes an image in parallel and ensures that all elements are evaluated.
computeP :: (Array arr cs e, Array RP cs e, Exchangable arr RP) =>
            Image arr cs e -> Image RP cs e
computeP !img = head $! do
  let img' = exchange RP img
  img' `deepSeqImage` return img'
{-# INLINE computeP #-}

-- | Computes an image sequentially and ensures that all elements are evaluated.
computeS :: (Array arr cs e, Array RS cs e, Exchangable arr RS) =>
            Image arr cs e -> Image RS cs e
computeS !img = head $! do
  let img' = exchange RS img
  img' `deepSeqImage` return img'
{-# INLINE computeS #-}

-- | Delays an image, so further operations can be fused together.
delay :: (ManifestArray arr cs e, Array RD cs e, Exchangable arr RD) =>
         Image arr cs e -> Image RD cs e
delay = exchange RD
{-# INLINE delay #-}


mult :: Array RD cs e => Image RD cs e -> Image RD cs e -> Image RD cs e
mult img1@(RUImage arr1) img2@(RUImage arr2) =
  if n1 /= m2
    then error $
         "Inner dimensions of multiplied images must be the same, but received: " ++
         show img1 ++ " X " ++ show img2
    else RDImage . R.fromFunction (Z :. m1 :. n2) $ getPx
  where
    (Z :. m1 :. n1) = R.extent arr1
    (Z :. m2 :. n2) = R.extent arr2
    getPx (Z :. i :. j) =
      R.sumAllS
        (R.slice arr1 (R.Any :. (i :: Int) :. R.All) R.*^
         R.slice arr2 (R.Any :. (j :: Int)))
    {-# INLINE getPx #-}
mult _ _ = _errorCompute "Graphics.Image.Interface.Repa.Internal.mult"
{-# INLINE mult #-}


shT2 :: DIM2 -> (Int, Int)
shT2 (Z :. i :. j) = (i, j)
{-# INLINE shT2 #-}

tSh2 :: (Int, Int) -> DIM2
tSh2 !(i, j) = Z :. i :. j 
{-# INLINE tSh2 #-}


suspendedComputeP :: Array RD cs e =>
                     Image RD cs e -> Image RP cs e
suspendedComputeP (RDImage arr) = RPImage . RUImage . R.suspendedComputeP $ arr
suspendedComputeP !img          = RPImage img
{-# INLINE suspendedComputeP #-}


getDelayed :: Array RD cs e => Image RD cs e -> R.Array R.D DIM2 (Pixel cs e)
getDelayed (RUImage arr) = R.delay arr
getDelayed (RDImage arr) = arr
getDelayed (RScalar px)  = R.fromFunction (Z :. 1 :. 1) (const px)
{-# INLINE getDelayed #-}


-- | Create an image from a 2D Repa delayed array.
fromRepaArray :: R.Array R.D DIM2 (Pixel cs e) -> Image RD cs e
fromRepaArray = RDImage


-- | Retrieve an underlying Repa array from `RD` image type.
toRepaArray :: (ColorSpace cs, Unbox (PixelElt cs e)) =>
               Image RD cs e -> R.Array R.D DIM2 (Pixel cs e)
toRepaArray (RUImage arr) = R.delay arr
toRepaArray (RDImage arr) = arr
toRepaArray (RScalar px) = R.fromFunction (Z :. 1 :. 1) $ const px
  
_errorCompute :: String -> t
_errorCompute err =
  error $
  err ++ ": Image should be computed at ths point. Please report this error."
                         

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


