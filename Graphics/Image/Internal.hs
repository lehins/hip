{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, ViewPatterns, BangPatterns,
FlexibleInstances, InstanceSigs #-}

module Graphics.Image.Internal (
  compute, fold, sum, ref, refDefault, refMaybe, dims, rows, cols, make, 
  map, zipWith, traverse, traverse2, transpose, backpermute, crop,
  fromVector, fromLists, fromArray, toVector, toLists, toArray,
  maximum, minimum, normalize,
  Image(..), RepaStrategy(..)
  ) where

import Prelude hiding (map, zipWith, maximum, minimum, sum)
import qualified Prelude as P (floor)
import Graphics.Image.Interface hiding (Image)
import qualified Graphics.Image.Interface as D (Image)
import Data.Array.Repa.Eval (Elt)
import Data.Array.Repa as R hiding ((++), map, zipWith, traverse, traverse2,
                                    transpose, backpermute)
import qualified Data.Array.Repa as R (map, zipWith, traverse, traverse2,
                                       transpose, backpermute)
import Data.Vector.Unboxed (Unbox, Vector, fromList)


data Image px where
  ComputedImage  :: (Elt px, Unbox px, Pixel px) => (Array U DIM2 px) -> Image px
  AbstractImage  :: (Elt px, Unbox px, Pixel px) => (Array D DIM2 px) -> Image px
  SingletonImage :: (Elt px, Unbox px, Pixel px) => (Array U DIM2 px) -> Image px


data RepaStrategy img px where
  Sequential :: D.Image img px => RepaStrategy img px
  Parallel   :: D.Image img px => RepaStrategy img px


instance (Elt px, Unbox px, Pixel px) => Strategy RepaStrategy Image px where
  compute Sequential (AbstractImage arr) = ComputedImage $ computeS arr
  compute Parallel (AbstractImage arr) =  arr' `deepSeqArray` ComputedImage arr'
    where arr' = head $ computeP arr
  compute _ img = img
  {-# INLINE compute #-}

  fold Sequential op px (AbstractImage arr) = foldAllS op px arr
  fold Sequential op px (ComputedImage arr) = foldAllS op px arr
  fold Parallel op px (AbstractImage arr)   = head $ foldAllP op px arr
  fold Parallel op px (ComputedImage arr)   = head $ foldAllP op px arr
  fold _ op px (SingletonImage arr)         = foldAllS op px arr
  {-# INLINE fold #-}


instance (Elt px, Unbox px, Pixel px) => D.Image Image px where
  ref (ComputedImage  arr) r c = index arr (Z :. r :. c)
  ref (SingletonImage arr) _ _ = unsafeIndex arr (Z :. 0 :. 0)
  ref (AbstractImage  arr) 0 0 = unsafeIndex arr (Z :. 0 :. 0)
  ref (AbstractImage    _) _ _ =
    error "Only computed images can be referenced, call 'compute' on the Image."
  {-# INLINE ref #-}

  dims (AbstractImage (extent -> (Z :. r :. c))) = (r, c)
  dims (ComputedImage (extent -> (Z :. r :. c))) = (r, c)
  dims (SingletonImage _) = (1, 1)
  {-# INLINE dims #-}

  make m n f = AbstractImage . fromFunction (Z :. m :. n) $ g where
    g (Z :. r :. c) = f r c
  {-# INLINE make #-}
    
  map op (SingletonImage arr) = SingletonImage $ computeS $ R.map op arr
  map op (AbstractImage arr)  = AbstractImage $ R.map op arr
  map op (ComputedImage arr)  = AbstractImage $ R.map op arr
  {-# INLINE map #-}
  
  zipWith op (SingletonImage a1) (SingletonImage a2) =
    SingletonImage $ computeS $ fromFunction (Z :. 0 :. 0) (
      const (op (a1 ! (Z :. 0 :. 0)) (a2 ! (Z :. 0 :. 0))))
  zipWith op (SingletonImage a1) (AbstractImage a2) =
    AbstractImage $ R.map (op (a1 ! (Z :. 0 :. 0))) a2
  zipWith op i1@(AbstractImage _) i2@(SingletonImage _) = zipWith (flip op) i2 i1
  zipWith op (SingletonImage a1) (ComputedImage a2) =
    AbstractImage $ R.map (op (a1 ! (Z :. 0 :. 0))) a2
  zipWith op i1@(ComputedImage _) i2@(SingletonImage _) = zipWith (flip op) i2 i1
  zipWith op (getDelayed -> a1) (getDelayed -> a2) = AbstractImage $ R.zipWith op a1 a2
  {-# INLINE zipWith #-}
  
  traverse (getDelayed -> !arr) !newDims !newPx =
    AbstractImage $ R.traverse arr newExtent newPixel where
    newExtent !(Z :. m :. n) = uncurry ix2 $ newDims m n
    {-# INLINE newExtent #-}
    newPixel !getPx !(Z :. i :. j) = newPx (((.).(.)) getPx ix2) i j
    {-# INLINE newPixel #-}
      -- g i j = f (Z :. i :. j) == g = ((.).(.)) f ix2
  {-# INLINE traverse #-}

  traverse2 (getDelayed -> !arr1) (getDelayed -> !arr2) !newDims !newPx =
    AbstractImage $ R.traverse2 arr1 arr2 newExtent newPixel where
      newExtent !(Z :. m1 :. n1) !(Z :. m2 :. n2) = uncurry ix2 $ newDims m1 n1 m2 n2
      {-# INLINE newExtent #-}
      newPixel !getPx1 !getPx2 !(Z :. i :. j) =
        newPx (((.).(.)) getPx1 ix2) (((.).(.)) getPx2 ix2) i j
      {-# INLINE newPixel #-}
  {-# INLINE traverse2 #-}

  traverse3 (getDelayed -> !arr1) (getDelayed -> !arr2) (getDelayed -> !arr3) !newDims !newPx =
    AbstractImage $ R.traverse3 arr1 arr2 arr3 newExtent newPixel where
      newExtent !(Z :. m1 :. n1) !(Z :. m2 :. n2) !(Z :. m3 :. n3) =
        uncurry ix2 $ newDims m1 n1 m2 n2 m3 n3
      {-# INLINE newExtent #-}
      newPixel !getPx1 !getPx2 !getPx3 !(Z :. i :. j) =
        newPx (((.).(.)) getPx1 ix2) (((.).(.)) getPx2 ix2) (((.).(.)) getPx3 ix2) i j
      {-# INLINE newPixel #-}
  {-# INLINE traverse3 #-}

  transpose !img@(SingletonImage _) = img
  transpose (getDelayed -> !arr)    = AbstractImage . R.transpose $ arr
  {-# INLINE transpose #-}

  backpermute _ _ _ !img@(SingletonImage _) = img
  backpermute !m !n !newIndex (getDelayed -> !arr) =
    AbstractImage $ R.backpermute (Z :. m :. n) newShape arr where
      newShape !(Z :. i :. j) = uncurry ix2 $ newIndex i j
      {-# INLINE newShape #-}
  {-# INLINE backpermute #-}

  crop _ _ _ _ !img@(SingletonImage _) = img
  crop !i !j !m !n (getDelayed -> !arr) =
    AbstractImage $ extract (Z :. i :. j) (Z :. m :. n) arr
  {-# INLINE crop #-}
   
  fromLists !ls =
    (fromVector (length ls) (length $ head ls)) . fromList . concat $ ls
  {-# INLINE fromLists #-}


getDelayed :: Pixel px => Image px -> Array D DIM2 px
getDelayed (AbstractImage a1)  = a1
getDelayed (ComputedImage a1)  = delay a1
getDelayed (SingletonImage a1) = delay a1
{-# INLINE getDelayed #-}


instance (Num px, Elt px, Unbox px, Pixel px) => Num (Image px) where
  (+)           = zipWith (+)
  {-# INLINE (+) #-}
  
  (-)           = zipWith (-)
  {-# INLINE (-) #-}
  
  (*)           = zipWith (*)
  {-# INLINE (*) #-}
  
  abs           = map abs
  {-# INLINE abs #-}
  
  signum        = map signum
  {-# INLINE signum #-}
  
  fromInteger i =
    SingletonImage $ computeS $ fromFunction (Z :. 0 :. 0) (const . fromInteger $ i)
  {-# INLINE fromInteger#-}


instance (Fractional px, Elt px, Unbox px, Pixel px) => Fractional (Image px) where
  (/)            = zipWith (/)
  {-# INLINE (/) #-}
  
  fromRational r =
    SingletonImage $ computeS $ fromFunction (Z :. 0 :. 0) (const . fromRational $ r)
  {-# INLINE fromRational #-}


instance (Floating px, Elt px, Unbox px, Pixel px) => Floating (Image px) where
  pi    = SingletonImage $ computeS $ fromFunction (Z :. 0 :. 0) (const pi)
  {-# INLINE pi #-}
  
  exp   = map exp
  {-# INLINE exp #-}
  
  log   = map log
  {-# INLINE log#-}
  
  sin   = map sin
  {-# INLINE sin #-}
  
  cos   = map cos
  {-# INLINE cos #-}
  
  asin  = map asin
  {-# INLINE asin #-}
  
  atan  = map atan
  {-# INLINE atan #-}
  
  acos  = map acos
  {-# INLINE acos #-}
  
  sinh  = map sinh
  {-# INLINE sinh #-}
  
  cosh  = map cosh
  {-# INLINE cosh #-}
  
  asinh = map asinh
  {-# INLINE asinh #-}
  
  atanh = map atanh
  {-# INLINE atanh #-}
  
  acosh = map acosh
  {-# INLINE acosh #-}


instance (Elt px, Unbox px, Pixel px) => Show (Image px) where
  show img@(dims -> (m, n)) = "<Image "++(showType px)++": "++(show m)++"x"++(show n)++">"
    where px = ref img 0 0


-- | Convert an Unboxed Vector to an Image by supplying rows, columns and
-- a vector.
fromVector :: (Elt px, Unbox px, Pixel px) =>
              Int -> Int  -- ^ Image dimension @m@ rows and @n@ columns.
           -> Vector px   -- ^ Flat vector image rpresentation with length @m*n@
           -> Image px
fromVector m n v = fromArray $ delay $ fromUnboxed (Z :. m :. n) v
{-# INLINE fromVector #-}


-- | Convert an Image to a Vector of length: rows*cols
toVector :: (Elt px, Unbox px, Pixel px) =>
            RepaStrategy Image px
         -> Image px
         -> Vector px
toVector strat = toUnboxed . toArray strat
{-# INLINE toVector #-}

              
-- | Create an image from a Repa Array
fromArray :: (Elt px, Unbox px, Pixel px, Source r px) =>
             Array r DIM2 px
             -> Image px
fromArray arr = AbstractImage . delay $ arr
{-# INLINE fromArray #-}


toArray :: (Elt px, Unbox px, Pixel px) =>
           RepaStrategy Image px
           -> Image px
           -> Array U DIM2 px
toArray _ (SingletonImage arr) = arr
toArray _ (ComputedImage arr)  = arr
toArray strat img              = toArray strat $ compute strat img
{-# INLINE toArray #-}
