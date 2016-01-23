{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleInstances, GADTs, MultiParamTypeClasses, ViewPatterns #-}

module Graphics.Image.Accelerate.Internal (
  Image, InterpreterStrategy(..)
  ) where

import Prelude hiding (map, zipWith, maximum, minimum, sum)
--import qualified Prelude as P (map)
import HIP.Interface hiding (Convertible(..))
import Graphics.Image.Accelerate.Pixel (Pixel)
import Data.Typeable (typeOf)
import Data.Array.Accelerate as A hiding ((++), map, zipWith, fromIntegral)
import qualified Data.Array.Accelerate as A (map, zipWith)
import Data.Array.Accelerate.Interpreter


-- | Image that uses Accelerate Unboxed Arrays as an underlying representation.
data Image px where
  ArrayImage    :: Pixel px => !(Array DIM2 px)       -> Image px
  Singleton     :: Pixel px => !px                    -> Image px


-- | Image that uses Accelerate Unboxed Arrays as an underlying representation.
data AccImage px where
  AccArrayImage :: Pixel px => !(Acc (Array DIM2 px)) -> Image px
  AccSingleton  :: Pixel px => !(Acc (Scalar px))     -> Image px


data InterpreterStrategy img px where
  Interpreter :: (Pixel px, AImage img px) => InterpreterStrategy img px


instance Pixel px => Strategy InterpreterStrategy Image px where
  compute _ !(AccArrayImage arr) = ArrayImage $ run arr
  compute _ !img = img
  {-# INLINE compute #-}

  --{-# INLINE fold #-}

  --{-# INLINE toBoxedVector #-}


instance (Pixel px) => AImage Image px where
  --index (ArrayImage arr) !i !j = A.indexArray arr (Z :. i :. j)
  --index !img !i !j              = unsafeIndex img i j
  --{-# INLINE index #-}

  --unsafeIndex !(AccArrayImage arr) !i !j = arr A.! (index2 i j)
  unsafeIndex !(ArrayImage arr) !i !j    = A.indexArray arr (Z :. i :. j)
  unsafeIndex !(Singleton px)       _  _ = px
  --unsafeIndex !(DelayedImage arr)   0  0 = A.unsafeIndex arr (Z :. 0 :. 0)
  --unsafeIndex !(DelayedImage     _) _  _ =
  --  error "Only computed images can be referenced, call 'compute' on the Image."
  {-# INLINE unsafeIndex #-}

  --dims (AccArrayImage (extent -> (Z :. r :. c)))  = (r, c)
  dims (ArrayImage (arrayShape -> (Z :. r :. c))) = (r, c)
  dims (Singleton _)                              = (1, 1)
  {-# INLINE dims #-}
  
  make !m !n !f = ArrayImage $ fromFunction (Z :. m :. n) getPx where
    getPx (Z :. r :. c) = f r c
  {-# INLINE make #-}
    
{-
  map !op !(Singleton px)      = Singleton $ op px
  map !op !(DelayedImage arr)  = DelayedImage $ A.map op arr
  map !op !(ComputedImage arr) = DelayedImage $ A.map op arr
  {-# INLINE map #-}

  imap !op !(Singleton px)      = Singleton $ op 0 0 px
  imap !op !(getDelayed -> arr) = DelayedImage $ A.map op' arr' where
    !arr' = A.zipWith (,) (A.fromFunction (extent arr) id) arr
    op' !(Z :. i :. j, px) = op i j px 
  {-# INLINE imap #-}

  zipWith !op !(Singleton px1)    !(Singleton px2)    = Singleton $ op px1 px2
  zipWith !op !(Singleton px1)    !(getDelayed -> a2) = DelayedImage $ A.map (op px1) a2
  zipWith !op !(getDelayed -> a1) !(Singleton px2)    = DelayedImage $ A.map (`op` px2) a1
  zipWith !op !(getDelayed -> a1) !(getDelayed -> a2) = DelayedImage $ A.zipWith op a1 a2
  {-# INLINE zipWith #-}
  
  traverse !(getDelayed -> arr) !newDims !newPx =
    DelayedImage $ A.traverse arr newExtent newPixel where
    newExtent !(Z :. m :. n) = uncurry ix2 $ newDims m n
    {-# INLINE newExtent #-}
    newPixel !getPx !(Z :. i :. j) = newPx (((.).(.)) getPx ix2) i j
    {-# INLINE newPixel #-}
      -- "g i j = f (Z :. i :. j)" is equivalent to "g = ((.).(.)) f ix2"
  {-# INLINE traverse #-}

  traverse2 !(getDelayed -> arr1) !(getDelayed -> arr2) !newDims !newPx =
    DelayedImage $ A.traverse2 arr1 arr2 newExtent newPixel where
      newExtent !(Z :. m1 :. n1) !(Z :. m2 :. n2) = uncurry ix2 $ newDims m1 n1 m2 n2
      {-# INLINE newExtent #-}
      newPixel !getPx1 !getPx2 !(Z :. i :. j) =
        newPx (((.).(.)) getPx1 ix2) (((.).(.)) getPx2 ix2) i j
      {-# INLINE newPixel #-}
  {-# INLINE traverse2 #-}

  traverse3 !(getDelayed -> arr1) !(getDelayed -> arr2) !(getDelayed -> arr3) !newDims !newPx =
    DelayedImage $ A.traverse3 arr1 arr2 arr3 newExtent newPixel where
      newExtent !(Z :. m1 :. n1) !(Z :. m2 :. n2) !(Z :. m3 :. n3) =
        uncurry ix2 $ newDims m1 n1 m2 n2 m3 n3
      {-# INLINE newExtent #-}
      newPixel !getPx1 !getPx2 !getPx3 !(Z :. i :. j) =
        newPx (((.).(.)) getPx1 ix2) (((.).(.)) getPx2 ix2) (((.).(.)) getPx3 ix2) i j
      {-# INLINE newPixel #-}
  {-# INLINE traverse3 #-}

  transpose img@(Singleton _)    = img
  transpose !(getDelayed -> arr) = DelayedImage . A.transpose $ arr
  {-# INLINE transpose #-}
  
  backpermute _ _ _ img@(Singleton _)              = img
  backpermute !m !n !newIndex !(getDelayed -> arr) =
    DelayedImage $ A.backpermute (Z :. m :. n) newShape arr where
      newShape !(Z :. i :. j) = uncurry ix2 $ newIndex i j
      {-# INLINE newShape #-}
  {-# INLINE backpermute #-}

  crop _ _ _ _ !img@(Singleton _)       = img
  crop !i !j !m !n !(getDelayed -> !arr) =
    DelayedImage $ extract (Z :. i :. j) (Z :. m :. n) arr
  {-# INLINE crop #-}
   
  fromLists !ls = if isSquare
                  then fromUnboxedVector m n . fromList . concat $ ls
                  else error "fromLists: Inner lists do not have uniform length."
    where
      !(m, n) = (length ls, length $ head ls)
      !isSquare = (n > 0) && (all (==n) $ P.map length ls)
  {-# INLINE fromLists #-}

  fromBoxedVector !m !n = fromUnboxedVector m n . convert 
  {-# INLINE fromBoxedVector #-}
  
  (|*|) !img1@(ComputedImage arr1) !img2@(ComputedImage arr2) =
    if n1 /= m2 
    then  error ("Inner dimensions of multiplied images must be the same, but received: "++
                 show img1 ++" X "++ show img2)
    else
      DelayedImage $ fromFunction (Z :. m1 :. n2) getPx where
        !(Z :. m1 :. n1) = extent arr1
        !(Z :. m2 :. n2) = extent arr2
        getPx !(Z:. i :. j) =
          sumAllS (slice arr1 (Any :. (i :: Int) :. All) *^ slice arr2 (Any :. (j :: Int)))
  (|*|) _ _ = error "Images should be computed before they can be multiplied."
  {-# INLINE (|*|) #-}
  

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




getDelayed :: Image px -> Array D DIM2 px
getDelayed !(ComputedImage arr) = delay arr
getDelayed !(DelayedImage arr)  = arr
getDelayed !(Singleton px)      = fromFunction (Z :. 0 :. 0) (const px)
{-# INLINE getDelayed #-}


-- | Convert an Unboxed Vector to an Image by supplying rows, columns and
-- a vector.
fromUnboxedVector :: Pixel px =>
                     Int       -- ^ Image dimensions @m@ rows
                  -> Int       -- ^ and @n@ columns.
                  -> Vector px -- ^ Flat vector image rpresentation with length @m*n@
                  -> Image px
fromUnboxedVector !m !n !v = ComputedImage $ fromUnboxed (Z :. m :. n) v
{-# INLINE fromUnboxedVector #-}


-- | Convert an Image to an Unboxed Vector of length: rows*cols
toUnboxedVector :: Pixel px =>
                   AccelerateStrategy Image px
                -> Image px
                -> Vector px
toUnboxedVector !strat = toUnboxed . toArray strat
{-# INLINE toUnboxedVector #-}

              
-- | Create an image from a Accelerate Array
fromArray :: (Source r px, Pixel px) =>
             Array r DIM2 px
             -> Image px
fromArray !arr = DelayedImage . delay $ arr
{-# INLINE fromArray #-}


toArray :: Pixel px =>
           AccelerateStrategy Image px
           -> Image px
           -> Array U DIM2 px
toArray _ (Singleton !px)      = fromUnboxed (Z :. 0 :. 0) $ singleton px
toArray _ (ComputedImage !arr) = arr
toArray !strat !img            = toArray strat $ compute strat img
{-# INLINE toArray #-}


-}

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


instance Pixel px => Show (Image px) where
  show img@(dims -> (m, n)) =
    "<Image "++(show . typeOf $ index img 0 0)++": "++(show m)++"x"++(show n)++">"
