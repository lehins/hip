{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleInstances, GADTs, MultiParamTypeClasses, ViewPatterns #-}

module Graphics.Image.Repa.Internal (
  compute, fold, index, unsafeIndex, defaultIndex, maybeIndex, dims, rows, cols, make, 
  map, imap, zipWith, traverse, traverse2, traverse3, transpose, backpermute, crop,
  fromVector, fromLists, fromArray, toVector, toLists, toArray,
  maximum, minimum, normalize, sum,
  Image(..), RepaStrategy(..)
  ) where

import Prelude hiding (map, zipWith, maximum, minimum, sum)
import qualified Prelude as P (map)
import Graphics.Image.Interface hiding (Pixel)
import Graphics.Image.Repa.Pixel (Pixel)
import Data.Array.Repa as R hiding (
  (++), index, unsafeIndex, map, zipWith, traverse, traverse2, traverse3,
  transpose, backpermute)
import qualified Data.Array.Repa as R (
  index, unsafeIndex, map, zipWith, traverse, traverse2, traverse3,
  transpose, backpermute)
import Data.Vector.Unboxed (Unbox, Vector, fromList, singleton)


{- | Image that uses Repa Unboxed Array as an underlying representation. -}
data Image px where
  ComputedImage  :: (Unbox px, Pixel px) => !(Array U DIM2 px) -> Image px
  AbstractImage  :: Pixel px => !(Array D DIM2 px) -> Image px
  Singleton      :: Pixel px => !px -> Image px


data RepaStrategy img px where
  Sequential :: (Pixel px, AImage img px) => RepaStrategy img px
  Parallel   :: (Pixel px, AImage img px) => RepaStrategy img px


instance Pixel px => Strategy RepaStrategy Image px where
  compute Sequential (AbstractImage !arr) = ComputedImage $ computeS arr
  compute Parallel (AbstractImage !arr) =  arr' `deepSeqArray` ComputedImage arr'
    where !arr' = head $ computeP arr
  compute _ !img = img
  {-# INLINE compute #-}

  fold Sequential op px (AbstractImage arr) = foldAllS op px arr
  fold Sequential op px (ComputedImage arr) = foldAllS op px arr
  fold Parallel op px (AbstractImage arr)   = head $ foldAllP op px arr
  fold Parallel op px (ComputedImage arr)   = head $ foldAllP op px arr
  fold _ op px' (Singleton px)              = op px' px
  {-# INLINE fold #-}


instance (Pixel px) => AImage Image px where
  index (ComputedImage !arr) !i !j = R.index arr (Z :. i :. j)
  index !img !i !j                 = unsafeIndex img i j
  {-# INLINE index #-}

  unsafeIndex (ComputedImage  !arr) i j = R.unsafeIndex arr (Z :. i :. j)
  unsafeIndex (Singleton       !px) _ _ = px
  unsafeIndex (AbstractImage  !arr) 0 0 = R.unsafeIndex arr (Z :. 0 :. 0)
  unsafeIndex (AbstractImage     _) _ _ =
    error "Only computed images can be referenced, call 'compute' on the Image."
  {-# INLINE unsafeIndex #-}

  dims (AbstractImage (extent -> (Z :. r :. c))) = (r, c)
  dims (ComputedImage (extent -> (Z :. r :. c))) = (r, c)
  dims (Singleton _) = (1, 1)
  {-# INLINE dims #-}

  make m n f = AbstractImage . fromFunction (Z :. m :. n) $ g where
    g (Z :. r :. c) = f r c
  {-# INLINE make #-}
    
  map op (Singleton      px) = Singleton $ op px
  map op (AbstractImage arr) = AbstractImage $ R.map op arr
  map op (ComputedImage arr) = AbstractImage $ R.map op arr
  {-# INLINE map #-}

  zipWith op (Singleton px1) (Singleton px2) = Singleton $ op px1 px2
  zipWith op (Singleton px1) (getDelayed -> a2) = AbstractImage $ R.map (op px1) a2
  zipWith op (getDelayed -> a1) (Singleton px2) = AbstractImage $ R.map (flip op px2) a1
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

  transpose !img@(Singleton _)   = img
  transpose (getDelayed -> !arr) = AbstractImage . R.transpose $ arr
  {-# INLINE transpose #-}
  
  backpermute _ _ _ !img@(Singleton _)             = img
  backpermute !m !n !newIndex (getDelayed -> !arr) =
    AbstractImage $ R.backpermute (Z :. m :. n) newShape arr where
      newShape !(Z :. i :. j) = uncurry ix2 $ newIndex i j
      {-# INLINE newShape #-}
  {-# INLINE backpermute #-}

  crop _ _ _ _ !img@(Singleton _)       = img
  crop !i !j !m !n (getDelayed -> !arr) =
    AbstractImage $ extract (Z :. i :. j) (Z :. m :. n) arr
  {-# INLINE crop #-}
   
  fromLists !ls = if isSquare
                  then (fromVector m n) . fromList . concat $ ls
                  else error "fromLists: Inner lists do not have uniform length."
    where
      (m, n) = (length ls, length $ head ls)
      isSquare = (n > 0) && (all (==2) $ P.map length ls)
  {-# INLINE fromLists #-}


instance Pixel px => Num (Image px) where
  (+)         = zipWith (+)
  {-# INLINE (+) #-}
  
  (-)         = zipWith (-)
  {-# INLINE (-) #-}
  
  (*)         = zipWith (*)
  {-# INLINE (*) #-}
  
  abs         = map abs
  {-# INLINE abs #-}
  
  signum      = map signum
  {-# INLINE signum #-}
  
  fromInteger = Singleton . fromInteger
  {-# INLINE fromInteger#-}


instance (Fractional px, Pixel px) => Fractional (Image px) where
  (/)          = zipWith (/)
  {-# INLINE (/) #-}
  
  fromRational = Singleton . fromRational 
  {-# INLINE fromRational #-}


instance (Floating px, Pixel px) => Floating (Image px) where
  pi    = Singleton pi
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
    where px = index img 0 0


getDelayed :: Image px -> Array D DIM2 px
getDelayed (AbstractImage arr)  = arr
getDelayed (ComputedImage arr)  = delay arr
getDelayed (Singleton px)       = fromFunction (Z :. 0 :. 0) (const px)
{-# INLINE getDelayed #-}


-- | Convert an Unboxed Vector to an Image by supplying rows, columns and
-- a vector.
fromVector :: Pixel px =>
              Int  -- ^ Image dimensions @m@ rows
           -> Int  -- ^ and @n@ columns.
           -> Vector px   -- ^ Flat vector image rpresentation with length @m*n@
           -> Image px
fromVector m n v = fromArray $ delay $ fromUnboxed (Z :. m :. n) v
{-# INLINE fromVector #-}


-- | Convert an Image to a Vector of length: rows*cols
toVector :: Pixel px =>
            RepaStrategy Image px
         -> Image px
         -> Vector px
toVector strat = toUnboxed . toArray strat
{-# INLINE toVector #-}

              
-- | Create an image from a Repa Array
fromArray :: (Source r px, Pixel px) =>
             Array r DIM2 px
             -> Image px
fromArray arr = AbstractImage . delay $ arr
{-# INLINE fromArray #-}


toArray :: Pixel px =>
           RepaStrategy Image px
           -> Image px
           -> Array U DIM2 px
toArray _ (Singleton px)       = fromUnboxed (Z :. 0 :. 0) $ singleton px
toArray _ (ComputedImage arr)  = arr
toArray strat img              = toArray strat $ compute strat img
{-# INLINE toArray #-}


