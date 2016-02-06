{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, ConstraintKinds, GADTs, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Repa.Internal (
  RD(..), RP(..), RS(..), computeP, computeS
  ) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P (map)
import Graphics.Image.Interface

import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Vector.Unboxed.Deriving
import Data.Array.Repa hiding (
  Array, map, zipWith, rank, index, traverse, (++), computeP, computeS,
  traverse, transpose, backpermute)
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R (Elt(..), suspendedComputeP)


-- | Repa Delayed Array representation.
data RD = RD

-- | Repa Unboxed Array representation, which is computed in parallel.
data RP = RP

-- | Repa Unboxed Array representation, which is computed sequentially. 
data RS = RS

instance Show RD where
  show _ = "RepaDelayed"

instance Show RP where
  show _ = "RepaParallel"
  
instance Show RS where
  show _ = "RepaSequential"

instance Elt RD cs e => Array RD cs e where
  type Elt RD cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))

  data Image RD cs e where
    RScalar :: !(Pixel cs e)                  -> Image RD cs e
    RUImage :: !(R.Array U DIM2 (Pixel cs e)) -> Image RD cs e
    RDImage :: !(R.Array D DIM2 (Pixel cs e)) -> Image RD cs e

  dims (RScalar _                        ) = (1, 1)
  dims (RUImage (extent -> (Z :. m :. n))) = (m, n)
  dims (RDImage (extent -> (Z :. m :. n))) = (m, n)
  {-# INLINE dims #-}

  index (RScalar px)  (0, 0) = px
  index (RScalar _)   (_, _) = error "Scalar Image can only be indexed at (0,0)."
  index (RUImage arr) (i, j) = R.index arr (Z :. i :. j)
  index (RDImage arr) (i, j) = R.index arr (Z :. i :. j)
  {-# INLINE index #-}

  singleton = RScalar
  {-# INLINE singleton #-}

  make !(m, n) !f = RDImage $ fromFunction (Z :. m :. n) (f . ixT2)
  {-# INLINE make #-}

  map f (RScalar px)        = RScalar (f px)
  map f (getDelayed -> arr) = RDImage (R.map f arr)

  imap f (RScalar px)  = RScalar (f (0, 0) px)
  imap f (getDelayed -> arr) = RDImage (R.zipWith f (R.fromFunction (extent arr) ixT2) arr)
    
  zipWith f (RScalar px1)        (RScalar px2)        = RScalar (f px1 px2)
  zipWith f (RScalar px1)        (getDelayed -> arr2) = RDImage (R.map (f   px1) arr2)
  zipWith f (getDelayed -> arr1) (RScalar px2)        = RDImage (R.map (`f` px2) arr1)
  zipWith f (getDelayed -> arr1) (getDelayed -> arr2) = RDImage (R.zipWith f arr1 arr2)

  traverse (getDelayed -> arr) newDims newPx =
    RDImage $ R.traverse arr (tIx2 . newDims . ixT2) newPixel where
    newPixel getPx = newPx (getPx . tIx2) . ixT2

  transpose img@(RScalar _)     = img
  transpose (RDImage arr) = RDImage (R.transpose arr)
  transpose (RUImage arr) = RDImage (R.transpose arr)

  backpermute _ _ img@(RScalar _)                = img
  backpermute (tIx2 -> sh) g (getDelayed -> arr) =
    RDImage (R.backpermute sh (tIx2 . g . ixT2) arr)


instance Elt RS cs e => Array RS cs e where
  type Elt RS cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RS cs e where
    RSImage :: !(Image RD cs e) -> Image RS cs e

  dims (RSImage img) = dims img

  index (RSImage img) = index img

  make ix = computeS . make ix

  singleton = RSImage . singleton

  map f (RSImage img) = computeS . map f $ img

  imap f (RSImage img) = computeS . imap f $ img

  zipWith f (RSImage img1) (RSImage img2) = computeS . zipWith f img1 $ img2

  traverse (RSImage img) newDims = computeS . traverse img newDims 

  transpose (RSImage img) = computeS . transpose $ img
  
  backpermute f g (RSImage img) = computeS $ backpermute f g img



instance Elt RP cs e => Array RP cs e where
  type Elt RP cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RP cs e where
    RPImage :: !(Image RD cs e) -> Image RP cs e

  dims (RPImage img) = dims img
  {-# INLINE dims #-}

  index (RPImage img) = index img
  {-# INLINE index #-}

  make !ix = suspendedComputeP . make ix
  {-# INLINE make #-}

  singleton = RPImage . singleton
  {-# INLINE singleton #-}

  map f (RPImage img) = suspendedComputeP . map f $ img

  imap f (RPImage img) = suspendedComputeP . imap f $ img

  zipWith f (RPImage img1) (RPImage img2) = suspendedComputeP . zipWith f img1 $ img2

  traverse (RPImage img) newDims = suspendedComputeP . traverse img newDims 

  transpose (RPImage img) = suspendedComputeP . transpose $ img
  
  backpermute !f !g (RPImage img) = suspendedComputeP $ backpermute f g img



instance Transformable RD RS where    

  transform img RS = computeS img

instance Transformable RD RP where
  
  transform img RP = suspendedComputeP img


instance Transformable RS RD where

  transform (RSImage img) RD = img

instance Transformable RP RD where
  
  transform (RPImage img) RD = img


instance Transformable RS RP where
  
  transform (RSImage img) RP = RPImage img

instance Transformable RP RS where
  
  transform (RPImage img) RS = RSImage img



instance Array RS cs e => ManifestArray RS cs e where

  deepSeqImage (RSImage (RUImage arr)) = deepSeqArray arr
  {-# INLINE deepSeqImage #-}

  (|*|) i1@(RSImage img1) i2@(RSImage img2) =
    i1 `deepSeqImage` i2 `deepSeqImage` computeS (mult img1 img2)
  {-# INLINE (|*|) #-}

  fold !f !a (RSImage (RUImage arr)) = R.foldAllS f a $ arr
  {-# INLINE fold #-}

  eq (RSImage (RUImage arr1)) (RSImage (RUImage arr2)) = R.equalsS arr1 arr2
  {-# INLINE eq #-}


instance Array RP cs e => ManifestArray RP cs e where

  deepSeqImage (RPImage (RUImage arr)) = deepSeqArray arr
  {-# INLINE deepSeqImage #-}

  (|*|) i1@(RPImage img1) i2@(RPImage img2) =
    i1 `deepSeqImage` i2 `deepSeqImage` suspendedComputeP (mult img1 img2)
  {-# INLINE (|*|) #-}

  fold !f !a (RPImage (RUImage arr)) = head . R.foldAllP f a $ arr
  {-# INLINE fold #-}

  eq (RPImage (RUImage arr1)) (RPImage (RUImage arr2)) = head $ R.equalsP arr1 arr2
  {-# INLINE eq #-}




mult img1@(RUImage arr1) img2@(RUImage arr2) =
  if n1 /= m2 
  then  error ("Inner dimensions of multiplied images must be the same, but received: "++
               show img1 ++" X "++ show img2)
    else
      RDImage . fromFunction (Z :. m1 :. n2) $ getPx where
        (Z :. m1 :. n1) = extent arr1
        (Z :. m2 :. n2) = extent arr2
        getPx (Z :. i :. j) =
          sumAllS (slice arr1 (Any :. (i :: Int) :. All) *^ slice arr2 (Any :. (j :: Int)))
{-# INLINE mult #-}


ixT2 :: DIM2 -> (Int, Int)
ixT2 !(Z :. i :. j) = (i, j)
{-# INLINE ixT2 #-}

tIx2 :: (Int, Int) -> DIM2
tIx2 !(i, j) = (Z :. i :. j) 
{-# INLINE tIx2 #-}

getDelayed :: Array RD cs e => Image RD cs e -> R.Array D DIM2 (Pixel cs e)
getDelayed (RUImage arr) = R.delay arr
getDelayed (RDImage arr) = arr
getDelayed _             = error "Scalar image is not an array."
{-# INLINE getDelayed #-}

  
suspendedComputeP :: Array RD cs e =>
                     Image RD cs e -> Image RP cs e
suspendedComputeP (RDImage arr) = RPImage . RUImage . R.suspendedComputeP $ arr
suspendedComputeP !img          = RPImage img
{-# INLINE suspendedComputeP #-}


computeP :: (Array RD cs e, Array RP cs e) =>
            Image RD cs e -> Image RP cs e
computeP (RDImage arr) = RPImage . RUImage . head . R.computeP $ arr
computeP !img          = RPImage img
{-# INLINE computeP #-}


computeS :: (Array RD cs e, Array RP cs e) =>
            Image RD cs e -> Image RS cs e
computeS (RDImage arr) = RSImage . RUImage . R.computeS $ arr
computeS !img          = RSImage img
{-# INLINE computeS #-}

                                                         
_error_traverse_scalar :: String
_error_traverse_scalar =
  "Traversal of a scalar image does not make sense, hence it is not implemented."

instance (ColorSpace cs, R.Elt e, Num e) => R.Elt (Pixel cs e) where
  touch !px = mapM_ (R.touch . getPxCh px) (enumFrom (toEnum 0)) 
  {-# INLINE touch #-}
  
  zero     = 0
  {-# INLINE zero #-}
  
  one      = 1
  {-# INLINE one #-}
  
derivingUnbox "Pixel"
    [t| (ColorSpace cs, Unbox (PixelElt cs e)) => (Pixel cs e) -> (PixelElt cs e) |]
    [| toElt                                                                      |]
    [| fromElt                                                                    |]
