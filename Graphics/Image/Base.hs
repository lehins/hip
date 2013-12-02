{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

module Graphics.Image.Base (
  Image, 
  Processable(..),
  Pixel(..), Convertable(..),
  getDelayed, fromDelayed, getComputed, fromComputed
  ) where

import Graphics.Image.Internal
import Data.Array.Repa as R
import Data.Array.Repa.Eval as R
import qualified Data.Vector.Unboxed as V

      
data Image px = ComputedImage  !(Array U DIM2 px)
              | DelayedImage !(Array D DIM2 px)
              | PureImage    !(Array D DIM0 px)


instance Pixel px => Processable Image px where
  dims (DelayedImage arr) = (r, c) where (Z :. r :. c) = extent arr
  dims (ComputedImage arr) = (r, c) where (Z :. r :. c) = extent arr
  {-# INLINE dims #-}

  ref (DelayedImage arr) r c = index arr (Z :. r :. c)
  ref (ComputedImage arr) r c = index arr (Z :. r :. c)
  {-# INLINE ref #-}
  
  make m n f = DelayedImage . fromFunction (Z :. m :. n) $ g where
    g (Z :. m :. n) = f m n
  {-# INLINE make #-}
    
  map = imgMap
  {-# INLINE map #-}
  
  zipWith = imgZipWith
  {-# INLINE zipWith #-}
  
  fold = imgFold
  {-# INLINE fold #-}
  
  traverse = imgTraverse
  {-# INLINE traverse #-}

  fromVector r c = ComputedImage . (fromUnboxed (Z :. r :. c))
  {-# INLINE fromVector #-}
  
  toVector = toUnboxed . getComputed
  {-# INLINE toVector #-}



isSmall :: (Num a, Ord a, Source r e) => Array r DIM2 e -> Bool
{-# INLINE isSmall #-}
isSmall (extent -> (Z :. r  :. c)) = r*c < 150

getDelayed :: V.Unbox e => Image e -> Array D DIM2 e
{-# INLINE getDelayed #-}
getDelayed (ComputedImage arr)  = delay arr
getDelayed (DelayedImage arr) = arr


fromDelayed :: Array D DIM2 px -> Image px
{-# INLINE fromDelayed #-}
fromDelayed = DelayedImage

getComputed :: V.Unbox e => Image e -> Array U DIM2 e
{-# INLINE getComputed #-}
getComputed (ComputedImage arr) = arr
getComputed (DelayedImage arr)
  | isSmall arr = head . computeUnboxedP $ arr
  | otherwise = computeUnboxedS arr

fromComputed :: [Array U DIM2 px] -> Image px
{-# INLINE fromComputed #-}
fromComputed = ComputedImage . head

imgMap :: V.Unbox a => (a -> px) -> Image a -> Image px
{-# INLINE imgMap #-}
imgMap op (PureImage arr)    = PureImage $ R.map op arr
imgMap op img = DelayedImage . (R.map op) . getDelayed $ img

imgZipWith :: (V.Unbox a, V.Unbox b) =>
              (a -> b -> px) -> Image a -> Image b -> Image px
{-# INLINE imgZipWith #-}
imgZipWith op (PureImage arr1) (PureImage arr2) =
  PureImage $ fromFunction Z (const (op (arr1 ! Z) (arr2 ! Z)))
imgZipWith op (PureImage arr) img2 =
  DelayedImage $ R.map (op (arr ! Z)) (getDelayed img2)
imgZipWith op img1 (PureImage arr) =
  DelayedImage $ R.map (flip op (arr ! Z)) (getDelayed img1)
imgZipWith op img1 img2 =
  DelayedImage $ R.zipWith op (getDelayed img1) (getDelayed img2)


imgFold :: (Elt a, V.Unbox a) => (a -> a -> a) -> a -> Image a -> a
{-# INLINE imgFold #-}
imgFold op px (getDelayed -> arr)
  | isSmall arr = foldAllS op px arr
  | otherwise   = head $ foldAllP op px arr

{-# INLINE imgTraverse #-}
imgTraverse img f g = DelayedImage $ R.traverse (getComputed img) f' g' where
  toShape i j = (Z :. i :. j)
  f' (Z :. m :. n) = uncurry toShape (f m n)
  g' u' (Z :. i' :. j') = g u i' j' where
    u i j = u' (Z :. i :. j)

instance (V.Unbox px, Num px) => Num (Image px) where
  (+) = imgZipWith (+)
  {-# INLINE (+) #-}
  
  (-) = imgZipWith (-)
  {-# INLINE (-) #-}
  
  (*) = imgZipWith (*)
  {-# INLINE (*) #-}
  
  abs = imgMap abs
  {-# INLINE abs #-}
  
  signum = imgMap signum
  {-# INLINE signum #-}
  
  fromInteger i = PureImage $ fromFunction Z (const . fromInteger $ i)
  {-# INLINE fromInteger#-}

instance (V.Unbox px, Fractional px) => Fractional (Image px) where
  (/) = imgZipWith (/)
  {-# INLINE (/) #-}
  
  fromRational r = PureImage $ fromFunction Z (const . fromRational $ r)
  {-# INLINE fromRational #-}

instance (V.Unbox px, Floating px) => Floating (Image px) where
  pi      = PureImage $ fromFunction Z (const pi)
  {-# INLINE pi #-}
  exp     = imgMap exp
  {-# INLINE exp #-}
  log     = imgMap log
  {-# INLINE log#-}
  sin     = imgMap sin
  {-# INLINE sin #-}
  cos     = imgMap cos
  {-# INLINE cos #-}
  asin    = imgMap asin
  {-# INLINE asin #-}
  atan    = imgMap atan
  {-# INLINE atan #-}
  acos    = imgMap acos
  {-# INLINE acos #-}
  sinh    = imgMap sinh
  {-# INLINE sinh #-}
  cosh    = imgMap cosh
  {-# INLINE cosh #-}
  asinh   = imgMap asinh
  {-# INLINE asinh #-}
  atanh   = imgMap atanh
  {-# INLINE atanh #-}
  acosh   = imgMap acosh
  {-# INLINE acosh #-}




  
