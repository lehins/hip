{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ViewPatterns #-}

module Graphics.Image.Internal where

import Prelude hiding ((++))
import qualified Prelude as P (floor)
import Graphics.Image.Definition
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as V
import qualified Codec.Picture as JP
import Data.Array.Repa as R
import qualified Data.Array.Repa as R (map)



instance Pixel px => Abstract Image px where
  dims (DelayedImage arr) = (r, c) where (Z :. r :. c) = extent arr
  dims (ComputedImage arr) = (r, c) where (Z :. r :. c) = extent arr
  dims (PureImage _) = (1, 1)
  {-# INLINE dims #-}

  make m n f = DelayedImage . fromFunction (Z :. m :. n) $ g where
    g (Z :. r :. c) = f r c
  {-# INLINE make #-}
    
  map = imgMap
  {-# INLINE map #-}
  
  zipWith = imgZipWith
  {-# INLINE zipWith #-}
  
  fromVector r c = ComputedImage . (fromUnboxed (Z :. r :. c))
  {-# INLINE fromVector #-}
  
  traverse = undefined
  

instance (V.Unbox px, Num px) => Num (Image px) where
  (+)           = imgZipWith (+)
  {-# INLINE (+) #-}
  
  (-)           = imgZipWith (-)
  {-# INLINE (-) #-}
  
  (*)           = imgZipWith (*)
  {-# INLINE (*) #-}
  
  abs           = imgMap abs
  {-# INLINE abs #-}
  
  signum        = imgMap signum
  {-# INLINE signum #-}
  
  fromInteger i =
    PureImage $ computeS $ fromFunction (Z :. 0 :. 0) (const . fromInteger $ i)
  {-# INLINE fromInteger#-}


instance (V.Unbox px, Fractional px) => Fractional (Image px) where
  (/)            = imgZipWith (/)
  {-# INLINE (/) #-}
  
  fromRational r =
    PureImage $ computeS $ fromFunction (Z :. 0 :. 0) (const . fromRational $ r)
  {-# INLINE fromRational #-}


instance (V.Unbox px, Floating px) => Floating (Image px) where
  pi    = PureImage $ computeS $ fromFunction (Z :. 0 :. 0) (const pi)
  {-# INLINE pi #-}
  
  exp   = imgMap exp
  {-# INLINE exp #-}
  
  log   = imgMap log
  {-# INLINE log#-}
  
  sin   = imgMap sin
  {-# INLINE sin #-}
  
  cos   = imgMap cos
  {-# INLINE cos #-}
  
  asin  = imgMap asin
  {-# INLINE asin #-}
  
  atan  = imgMap atan
  {-# INLINE atan #-}
  
  acos  = imgMap acos
  {-# INLINE acos #-}
  
  sinh  = imgMap sinh
  {-# INLINE sinh #-}
  
  cosh  = imgMap cosh
  {-# INLINE cosh #-}
  
  asinh = imgMap asinh
  {-# INLINE asinh #-}
  
  atanh = imgMap atanh
  {-# INLINE atanh #-}
  
  acosh = imgMap acosh
  {-# INLINE acosh #-}


imgMap :: (V.Unbox a, V.Unbox px) => (a -> px) -> Image a -> Image px
imgMap op (PureImage arr)     = PureImage $ computeS $ R.map op arr
imgMap op (DelayedImage arr)  = DelayedImage $ R.map op arr
imgMap op (ComputedImage arr) = DelayedImage $ R.map op arr
{-# INLINE imgMap #-}


imgZipWith :: (V.Unbox a, V.Unbox b, V.Unbox px) =>
              (a -> b -> px) -> Image a -> Image b -> Image px
imgZipWith op (PureImage a1) (PureImage a2) =
  PureImage $ computeS $ fromFunction (Z :. 0 :. 0) (
    const (op (a1 ! (Z :. 0 :. 0)) (a2 ! (Z :. 0 :. 0))))
imgZipWith op (PureImage a1) (DelayedImage a2) = DelayedImage $ R.map (op (a1 ! (Z :. 0 :. 0))) a2
imgZipWith op i1@(DelayedImage _) i2@(PureImage _) = imgZipWith (flip op) i2 i1
imgZipWith op (PureImage a1) (ComputedImage a2) = DelayedImage $ R.map (op (a1 ! (Z :. 0 :. 0))) a2
imgZipWith op i1@(ComputedImage _) i2@(PureImage _) = imgZipWith (flip op) i2 i1
imgZipWith op (ComputedImage a1) (DelayedImage a2) = DelayedImage $ R.zipWith op a1 a2
imgZipWith op (DelayedImage a1) (ComputedImage a2) = DelayedImage $ R.zipWith op a1 a2
imgZipWith op (ComputedImage a1) (ComputedImage a2) = DelayedImage $ R.zipWith op a1 a2
imgZipWith op (DelayedImage a1) (DelayedImage a2) = DelayedImage $ R.zipWith op a1 a2
{-# INLINE imgZipWith #-}

