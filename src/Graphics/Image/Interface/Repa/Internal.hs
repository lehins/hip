{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, ConstraintKinds, GADTs, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies,
             UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Interface.Repa.Internal (
  RD(..), RP(..), RS(..), computeP, computeS, delay
  ) where

import Prelude hiding (map, zipWith, foldl, foldr, mapM, mapM_, read)
import qualified Prelude as P (map, mapM_)
import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Binary (Bit(..))
import Graphics.Image.Interface.Vector.Unboxed (VU(..), fromUnboxedVector, toUnboxedVector)
import Control.Monad (liftM)
import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V ((!))
import Data.Function (on)
import Data.Typeable (Typeable)
import Data.Array.Repa hiding (
  Array, map, zipWith, rank, index, traverse, (++), computeP, computeS, delay,
  traverse, traverse2, transpose, backpermute)
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
  type Elt RD cs e = (ColorSpace cs, Num e, Typeable e,
                      R.Elt e, Unbox e, 
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

  singleton = RScalar
  {-# INLINE singleton #-}

  make !(m, n) !f = RDImage $ fromFunction (Z :. m :. n) (f . shT2)
  {-# INLINE make #-}

  map f (RScalar px)        = RScalar (f px)
  map f (getDelayed -> arr) = RDImage (R.map f arr)
  {-# INLINE map #-}

  imap f (RScalar px)  = RScalar (f (0, 0) px)
  imap f (getDelayed -> arr) = RDImage (R.zipWith f (R.fromFunction (extent arr) shT2) arr)
  {-# INLINE imap #-}
    
  zipWith f (RScalar px1)        (RScalar px2)        = RScalar (f px1 px2)
  zipWith f (RScalar px1)        (getDelayed -> arr2) = RDImage (R.map (f   px1) arr2)
  zipWith f (getDelayed -> arr1) (RScalar px2)        = RDImage (R.map (`f` px2) arr1)
  zipWith f (getDelayed -> arr1) (getDelayed -> arr2) = RDImage (R.zipWith f arr1 arr2)
  {-# INLINE zipWith #-}

  izipWith f (RScalar px1)        (RScalar px2)        = RScalar (f (0, 0) px1 px2)
  izipWith f (RScalar px1)        !img2                = imap (flip f px1) img2
  izipWith f !img1                (RScalar px2)        = imap (\ !ix !px -> f ix px px2) img1
  izipWith f (getDelayed -> arr1) (getDelayed -> arr2) =
    RDImage (R.traverse2 arr1 arr2 const getNewPx) where
      getNewPx !getPx1 !getPx2 !sh = f (shT2 sh) (getPx1 sh) (getPx2 sh)
      {-# INLINE getNewPx #-}
  {-# INLINE izipWith #-}

  traverse (getDelayed -> arr) newDims newPx =
    RDImage $ R.traverse arr (tSh2 . newDims . shT2) newPixel where
    newPixel getPx = newPx (getPx . tSh2) . shT2
  {-# INLINE traverse #-}

  traverse2 (getDelayed -> arr1) (getDelayed -> arr2) newDims newPx =
    RDImage $ R.traverse2 arr1 arr2 (((.).(.)) tSh2 (newDims `on` shT2)) newPixel where
    newPixel getPx1 getPx2 = newPx (getPx1 . tSh2) (getPx2 . tSh2) . shT2
  {-# INLINE traverse2 #-}

  transpose (RDImage arr) = RDImage (R.transpose arr)
  transpose (RUImage arr) = RDImage (R.transpose arr)
  transpose img           = img
  {-# INLINE transpose #-}

  backpermute _ _ img@(RScalar _)                = img
  backpermute (tSh2 -> sh) g (getDelayed -> arr) =
    RDImage (R.backpermute sh (tSh2 . g . shT2) arr)
  {-# INLINE backpermute #-}

  fromLists !ls = if isSquare
                  then RUImage . R.fromListUnboxed (Z :. m :. n) . concat $ ls
                  else error "fromLists: Inner lists do not all have an equal length."
    where
      !(m, n) = (length ls, length $ head ls)
      !isSquare = (n > 0) && all (==n) (P.map length ls)
  {-# INLINE fromLists #-}
  


instance Elt RS cs e => Array RS cs e where
  type Elt RS cs e = (ColorSpace cs, 
                      R.Elt e, Unbox e, Num e, Typeable e,
                      R.Elt (PixelElt cs e), Unbox (PixelElt cs e),
                      R.Elt (Pixel cs e), Unbox (Pixel cs e))
  
  data Image RS cs e where
    RSImage :: !(Image RD cs e) -> Image RS cs e

  dims (RSImage img) = dims img
  {-# INLINE dims #-}

  make !ix !f = computeS $ (make ix f :: Image RD cs e)
  {-# INLINE make #-}

  singleton = RSImage . singleton
  {-# INLINE singleton #-}

  map !f (RSImage img) = computeS . map f $ img
  {-# INLINE map #-}

  imap !f (RSImage img) = computeS . imap f $ img
  {-# INLINE imap #-}

  zipWith !f (RSImage img1) (RSImage img2) = computeS . zipWith f img1 $ img2
  {-# INLINE zipWith #-}

  izipWith !f (RSImage img1) (RSImage img2) = computeS . izipWith f img1 $ img2
  {-# INLINE izipWith #-}

  traverse (RSImage img) newDims = computeS . traverse img newDims 
  {-# INLINE traverse #-}

  traverse2 (RSImage img1) (RSImage img2) newDims = computeS . traverse2 img1 img2 newDims 
  {-# INLINE traverse2 #-}

  transpose (RSImage img) = computeS . transpose $ img
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
  
  data Image RP cs e where
    RPImage :: !(Image RD cs e) -> Image RP cs e

  dims (RPImage img) = dims img
  {-# INLINE dims #-}

  make !ix = suspendedComputeP . make ix
  {-# INLINE make #-}

  singleton = RPImage . singleton
  {-# INLINE singleton #-}

  map !f (RPImage img) = suspendedComputeP . map f $ img
  {-# INLINE map #-}

  imap !f (RPImage img) = suspendedComputeP . imap f $ img
  {-# INLINE imap #-}

  zipWith !f (RPImage img1) (RPImage img2) = suspendedComputeP . zipWith f img1 $ img2
  {-# INLINE zipWith #-}

  izipWith !f (RPImage img1) (RPImage img2) = suspendedComputeP . izipWith f img1 $ img2
  {-# INLINE izipWith #-}

  traverse (RPImage img) newDims = suspendedComputeP . traverse img newDims 
  {-# INLINE traverse #-}

  traverse2 (RPImage img1) (RPImage img2) newDims =
    suspendedComputeP . traverse2 img1 img2 newDims 
  {-# INLINE traverse2 #-}

  transpose (RPImage img) = suspendedComputeP . transpose $ img
  {-# INLINE transpose #-}
  
  backpermute !f !g (RPImage img) = suspendedComputeP $ backpermute f g img
  {-# INLINE backpermute #-}

  fromLists = RPImage . fromLists
  {-# INLINE fromLists #-}


  
instance Array RS cs e => ManifestArray RS cs e where

  index (RSImage (RUImage arr)) (i, j) = R.index arr (Z :. i :. j)
  index (RSImage (RScalar px))  (_, _) = px
  index _ _ = _error_compute
  {-# INLINE index #-}

  deepSeqImage (RSImage (RUImage arr)) = deepSeqArray arr
  deepSeqImage (RSImage (RScalar px))  = seq px
  deepSeqImage _ = _error_compute
  {-# INLINE deepSeqImage #-}

  (|*|) i1@(RSImage img1) i2@(RSImage img2) =
    i1 `deepSeqImage` i2 `deepSeqImage` computeS (mult img1 img2)
  {-# INLINE (|*|) #-}

  fold !f !px0 (RSImage (RUImage arr)) = R.foldAllS f px0 $ arr
  fold !f !px0 (RSImage (RScalar px))  = f px0 px
  fold _  _  _ = _error_compute
  {-# INLINE fold #-}

  eq (RSImage (RUImage arr1)) (RSImage (RUImage arr2)) = R.equalsS arr1 arr2
  eq _ _ = _error_compute
  {-# INLINE eq #-}


instance Array RP cs e => ManifestArray RP cs e where

  index (RPImage (RUImage arr)) (i, j) = R.index arr (Z :. i :. j)
  index (RPImage (RScalar px))  (0, 0) = px
  index (RPImage (RScalar _))   (_, _) = error "Scalar Image can only be indexed at (0,0)."
  index _ _ = _error_compute
  {-# INLINE index #-}

  deepSeqImage (RPImage (RUImage arr)) = deepSeqArray arr
  deepSeqImage (RPImage (RScalar px))  = seq px
  deepSeqImage _ = _error_compute
  {-# INLINE deepSeqImage #-}

  (|*|) i1@(RPImage img1) i2@(RPImage img2) =
    i1 `deepSeqImage` i2 `deepSeqImage` suspendedComputeP (mult img1 img2)
  {-# INLINE (|*|) #-}

  fold !f !px0 (RPImage (RUImage arr)) = head . R.foldAllP f px0 $ arr
  fold !f !px0 (RPImage (RScalar px))  = f px0 px
  fold _  _  _ = _error_compute
  {-# INLINE fold #-}

  eq (RPImage (RUImage arr1)) (RPImage (RUImage arr2)) = head $ R.equalsP arr1 arr2
  eq _ _ = _error_compute
  {-# INLINE eq #-}

  
instance ManifestArray RS cs e => SequentialArray RS cs e where

  foldl !f !a = foldl f a . exchange VU
  {-# INLINE foldl #-}

  foldr !f !a = foldr f a . exchange VU
  {-# INLINE foldr #-}

  mapM !f img = liftM (exchange RS) (mapM f (exchange VU img))
  {-# INLINE mapM #-}

  mapM_ !f img = mapM_ f (exchange VU img)
  {-# INLINE mapM_ #-}

  foldM !f !a = foldM f a . exchange VU
  {-# INLINE foldM #-}

  foldM_ !f !a = foldM_ f a . exchange VU
  {-# INLINE foldM_ #-}


instance ManifestArray RS cs e => MutableArray RS cs e where

  data MImage st RS cs e where
    MRSImage :: MImage st VU cs e -> MImage st RS cs e

  mdims (MRSImage (mdims -> sz)) = sz
  {-# INLINE mdims #-}

  thaw img = liftM MRSImage (thaw (exchange VU img))
  {-# INLINE thaw #-}

  freeze (MRSImage mimg) = liftM (exchange RS) (freeze mimg)
  {-# INLINE freeze #-}

  new sz = liftM MRSImage (new sz)
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
  exchange _ _                     = _error_compute
  {-# INLINE exchange #-}


-- | O(1) - Changes to Vector representation.
instance Exchangable RP VU where
  exchange _ img@(RPImage (RUImage arr)) = fromUnboxedVector (dims img) (R.toUnboxed arr)
  exchange _ (RPImage (RScalar px)) = singleton px
  exchange _ _                     = _error_compute
  {-# INLINE exchange #-}

-- | Computes an image in parallel and ensures that all elements are evaluated.
computeP :: (Array arr cs e, Array RP cs e, Exchangable arr RP) =>
            Image arr cs e -> Image RP cs e
computeP !img = head $ do
  img' <- return $ exchange RP img
  img' `deepSeqImage` return img'
{-# INLINE computeP #-}

-- | Computes an image sequentially and ensures that all elements are evaluated.
computeS :: (Array arr cs e, Array RS cs e, Exchangable arr RS) =>
            Image arr cs e -> Image RS cs e
computeS !img = head $ do
  img' <- return $ exchange RS img
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
  then error ("Inner dimensions of multiplied images must be the same, but received: "++
              show img1 ++" X "++ show img2)
  else RDImage . fromFunction (Z :. m1 :. n2) $ getPx where
    (Z :. m1 :. n1) = extent arr1
    (Z :. m2 :. n2) = extent arr2
    getPx (Z :. i :. j) =
      sumAllS (slice arr1 (Any :. (i :: Int) :. All) *^ slice arr2 (Any :. (j :: Int)))
    {-# INLINE getPx #-}
mult _ _ = _error_compute
{-# INLINE mult #-}


shT2 :: DIM2 -> (Int, Int)
shT2 !(Z :. i :. j) = (i, j)
{-# INLINE shT2 #-}

tSh2 :: (Int, Int) -> DIM2
tSh2 !(i, j) = (Z :. i :. j) 
{-# INLINE tSh2 #-}


suspendedComputeP :: Array RD cs e =>
                     Image RD cs e -> Image RP cs e
suspendedComputeP (RDImage arr) = RPImage . RUImage . R.suspendedComputeP $ arr
suspendedComputeP !img          = RPImage img
{-# INLINE suspendedComputeP #-}


getDelayed :: Array RD cs e => Image RD cs e -> R.Array D DIM2 (Pixel cs e)
getDelayed (RUImage arr) = R.delay arr
getDelayed (RDImage arr) = arr
getDelayed _             = error "Scalar image is not an array."
{-# INLINE getDelayed #-}

  
_error_compute :: t
_error_compute = error "Image should be computed at ths point. Please report this error"
                         
_error_scalar_op :: t
_error_scalar_op =
  error "This operation is not allowed on scalar images."


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


