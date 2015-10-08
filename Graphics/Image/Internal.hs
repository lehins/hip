{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, ViewPatterns, FlexibleInstances #-}

module Graphics.Image.Internal (
  compute, fold, sum, ref, refDefault, refMaybe, dims, make, 
  map, zipWith, traverse, transpose, backpermute, crop,
  fromVector, fromLists, fromArray, toVector, toLists, toArray,
  maximum, minimum, normalize,
  Image(..), RepaStrategy(..)
  ) where

import Prelude hiding (map, zipWith, maximum, minimum, sum)
import qualified Prelude as P (floor)
import Graphics.Image.Interface hiding (Image)
import qualified Graphics.Image.Interface as D (Image)
import Data.Array.Repa.Eval
import Data.Array.Repa as R hiding ((++), map, zipWith, traverse, transpose, backpermute)
import qualified Data.Array.Repa as R (map, zipWith, traverse, transpose, backpermute)


data Image px = ComputedImage !(Array U DIM2 px)
              | AbstractImage !(Array D DIM2 px)
              | SingletonImage !(Array U DIM2 px)


data RepaStrategy img px where
  Sequential :: (D.Image img px, Pixel px) => RepaStrategy img px
  Parallel   :: (D.Image img px, Pixel px) => RepaStrategy img px

                  

instance Pixel px => Strategy RepaStrategy Image px where
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

  toArray _ (SingletonImage arr) = arr
  toArray _ (ComputedImage arr)  = arr
  toArray strat img              = toArray strat $ compute strat img
  {-# INLINE toArray #-}


instance Pixel px => D.Image Image px where
  ref (ComputedImage arr)  r c = index arr (Z :. r :. c)
  ref (SingletonImage arr) _ _ = unsafeIndex arr (Z :. 0 :. 0)
  ref (AbstractImage arr)  0 0 = unsafeIndex arr (Z :. 0 :. 0)
  ref (AbstractImage _)    _ _ =
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
  
  traverse (getDelayed -> arr) ndims nref = AbstractImage $ R.traverse arr nsh nindex where
    nsh (Z :. r :. c) = uncurry ix2 $ ndims r c
    {-# INLINE nsh #-}
    nindex f (Z :. r :. c) = nref (((.).(.)) f ix2) r c
    {-# INLINE nindex #-}
      -- g i j = f (Z :. i :. j) == g = ((.).(.)) f ix2
  {-# INLINE traverse #-}

  transpose img@(SingletonImage _) = img
  transpose (getDelayed -> arr)    = AbstractImage . R.transpose $ arr
  {-# INLINE transpose #-}

  backpermute _ _ _ img@(SingletonImage _) = img
  backpermute r c f (getDelayed -> arr) =
    AbstractImage $ R.backpermute (Z :. r :. c) g arr where
      g (Z :. i :. j) = uncurry ix2 $ f i j
  {-# INLINE backpermute #-}

  crop _ _ _ _ img@(SingletonImage _) = img
  crop i j m n (getDelayed -> arr) =
    AbstractImage $ extract (Z :. i :. j) (Z :. m :. n) arr
  {-# INLINE crop #-}
  
  fromArray arr = AbstractImage . delay $ arr
  {-# INLINE fromArray #-}
  
  

getDelayed :: Pixel px => Image px -> Array D DIM2 px
getDelayed (AbstractImage a1)  = a1
getDelayed (ComputedImage a1)  = delay a1
getDelayed (SingletonImage a1) = delay a1
{-# INLINE getDelayed #-}

instance Pixel px => Num (Image px) where
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


instance Pixel px => Fractional (Image px) where
  (/)            = zipWith (/)
  {-# INLINE (/) #-}
  
  fromRational r =
    SingletonImage $ computeS $ fromFunction (Z :. 0 :. 0) (const . fromRational $ r)
  {-# INLINE fromRational #-}


instance Pixel px => Floating (Image px) where
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


instance Pixel px => Show (Image px) where
  show img@(dims -> (m, n)) = "<Image "++(showType px)++": "++(show m)++"x"++(show n)++">"
    where px = ref img 0 0