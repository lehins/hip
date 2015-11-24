{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.Image.Internal (
  Image, VectorStrategy(..), toVector, fromVector
  ) where

import Prelude hiding (map, zipWith, length, all, head, concat, foldl)
import qualified Prelude as P (map, length, all, head, concat)
import Data.Vector.Unboxed hiding ((++), map, zipWith, unsafeIndex, fromList)
import qualified Data.Vector.Unboxed as V (unsafeIndex, length, fromList, zipWith, sum)
import HIP.Interface hiding (Pixel)
import Graphics.Image.Pixel (Pixel)

-- | This is a concrete representation of an image that can hold any of the
-- pixels that are an instance of a 'Pixel'. It is also installed in 'Num's,
-- 'Floating' and 'Fractional', so you can perform regular methematical
-- operations on these images. If an operation involves an image and a regular
-- number than this opertaion will be applied to each pixel of an image, on the
-- other hand if operation involves two images this operation will be applied to
-- respective pixels at same locations, hene those images must have the same
-- dimensions. For example here is how you can sum two images:
--
-- >>> centaurus <- readImageRGB "images/centaurus.jpg"
-- >>> cluster <- readImageRGB "images/cluster.jpg"
-- >>> writeImage "images/centaurus_cluster.jpg" ((centaurus + cluster) / 2) []
--
-- <<images/centaurus.jpg>> <<images/cluster.jpg>> <<images/centaurus_cluster.jpg>>
--
data Image px where
  VectorImage   :: Pixel px => Int -> Int -> Vector px -> Image px
  Singleton     :: Pixel px => px -> Image px


data VectorStrategy img px where
  Identity :: (Pixel px, AImage img px) => VectorStrategy img px


instance Pixel px => Strategy VectorStrategy Image px where
  compute _ !img = img
  {-# INLINE compute #-}

  fold    _ f a !img = foldl f a $ toVector img
  {-# INLINE fold #-}

  toBoxedVector _ img = convert $ toVector img
  {-# INLINE toBoxedVector #-}



toIndex :: Int -> Int -> Int -> Int
toIndex n i j = i * n + j
{-# INLINE toIndex #-}


fromIndex :: Int -> Int -> (Int, Int)
fromIndex n k = (k `div` n, k `mod` n)
{-# INLINE fromIndex #-}


instance (Pixel px) => AImage Image px where
  make m n getPx = VectorImage m n $ generate (m * n) (uncurry getPx . fromIndex n)
  {-# INLINE make #-}
  
  dims (VectorImage m n _) = (m, n)
  dims _                   = (1, 1)
  {-# INLINE dims #-}
  
  unsafeIndex (VectorImage _ n v) i j = V.unsafeIndex v $ toIndex n i j
  unsafeIndex (Singleton px)      _ _ = px
  {-# INLINE unsafeIndex #-}

  zipWith !f (Singleton !px1) (Singleton !px2)        = Singleton (f px1 px2)
  zipWith !f (Singleton !px) !img@(VectorImage _ _ _) = map (f px) img
  zipWith !f !img@(VectorImage _ _ _) (Singleton !px) = map ((flip f) px) img
  zipWith !f !img1@(VectorImage m1 n1 _) !img2@(VectorImage m2 n2 _) =
    if m1 /= m2 || n1 /= n2
    then error ("Images must be of the same dimensions, received: "++
                show img1++" and "++show img2++".")
    else make m1 n1 getPx where
      getPx !i !j = f (unsafeIndex img1 i j) (unsafeIndex img2 i j)
      {-# INLINE getPx #-}
  {-# INLINE zipWith #-}
  
  fromList !ls = if isSquare
                 then (fromVector m n) . V.fromList . P.concat $ ls
                 else error "fromLists: Inner lists do not have uniform length."
    where
      (m, n) = (P.length ls, P.length $ P.head ls)
      isSquare = (n > 0) && (P.all (==n) $ P.map P.length ls)
  {-# INLINE fromList #-}

  (|*|) !img1@(VectorImage m1 n1 v1) !img2 =
    if n1 /= m2 
    then  error ("Inner dimensions of multiplied images must be the same, but received: "++
                 show img1 ++" X "++ show img2)
    else
      make m1 n2 getPx where
        !(VectorImage n2 m2 v2) = transpose img2
        getPx !i !j =
          V.sum $ V.zipWith (*) (slice (i*n1) n1 v1) (slice (j*m2) m2 v2)
  (|*|) _ _ = error "Images should be computed before they can be multiplied."
  {-# INLINE (|*|) #-}


instance (Pixel px) => Num (Image px) where
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


  
instance (Pixel px) => Show (Image px) where
  show img@(dims -> (m, n)) = "<Image "++(showType px)++": "++(show m)++"x"++(show n)++">"
    where px = index img 0 0  
  

-- | Convert an image to a flattened 'Vector'. It is a O(1) opeartion.
--
-- >>> toVector $ make 3 2 (\i j -> Gray $ fromIntegral (i+j))
-- fromList [<Gray:(0.0)>,<Gray:(1.0)>,<Gray:(1.0)>,<Gray:(2.0)>,<Gray:(2.0)>,<Gray:(3.0)>]
--
toVector :: Pixel px => Image px -> Vector px
toVector (VectorImage _ _ v) = v
toVector (Singleton px)      = singleton px


-- | Construct a two dimensional image with @m@ rows and @n@ columns from a one
-- dimensional 'Vector' of length @k@. It is a O(1) opeartion. Make sure that @m
-- * n = k@.
--
-- >>> fromVector 200 300 $ generate 60000 (\i -> Gray $ fromIntegral i / 60000)
-- <Image Gray: 200x300>
--
-- <<images/grad_fromVector.png>>
-- 
fromVector :: Pixel px =>
            Int       -- ^ @m@ rows
         -> Int       -- ^ @n@ columns
         -> Vector px -- ^ Source vector
         -> Image px
fromVector m n v
  | m * n == V.length v = VectorImage m n v
  | otherwise = error "fromVector: m * n doesn't equal the length of a Vector."
