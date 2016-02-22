{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Interface (
  ColorSpace(..), Alpha(..), Array(..), ManifestArray(..), MutableArray(..), MImage(..),
  Transformable(..)
  ) where

import Prelude hiding (map, zipWith, sum, product)
import GHC.Exts (Constraint)
import Control.Monad.Primitive (PrimMonad (..))


class (ColorSpace (Opaque cs), ColorSpace cs) => Alpha cs where
  type Opaque cs
  
  getAlpha :: Pixel cs e -> e

  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e
  
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


class (Enum cs, Show cs) => ColorSpace cs where
  -- | Representation of a pixel, such that it can be an element of any Array
  type PixelElt cs e

  data Pixel cs e

  fromChannel :: e -> Pixel cs e

  fromElt :: PixelElt cs e -> Pixel cs e

  toElt :: Pixel cs e -> PixelElt cs e

  getPxCh :: Pixel cs e -> cs -> e
  
  chOp :: (cs -> e' -> e) -> Pixel cs e' -> Pixel cs e 

  chOp2 :: (cs -> e1 -> e2 -> e) -> Pixel cs e1 -> Pixel cs e2 -> Pixel cs e
  
  pxOp :: (e' -> e) -> Pixel cs e' -> Pixel cs e

  pxOp2 :: (e1 -> e2 -> e) -> Pixel cs e1 -> Pixel cs e2 -> Pixel cs e
                                 
  
class (Show arr, ColorSpace cs, Num (Pixel cs e), Num e, Elt arr cs e) =>
      Array arr cs e where
  
  type Elt arr cs e :: Constraint
  type Elt arr cs e = ()
  data Image arr cs e

  makeImage :: (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e

  singleton :: Pixel cs e -> Image arr cs e

  -- | Get dimensions of an image.
  --
  -- >>> frog <- readImageRGB "images/frog.jpg"
  -- >>> frog
  -- <Image RepaDelayed RGB: 200x320>
  -- >>> dims frog
  -- (200,320)
  --
  dims :: Image arr cs e -> (Int, Int)

  -- | Map a function overa a source image.
  map :: Array arr cs' e' =>
         (Pixel cs' e' -> Pixel cs e)
         -- ^ A function that takes a pixel of a source image and returns a pixel
         -- for the result image a the same location.
      -> Image arr cs' e' -- ^ Source image.
      -> Image arr cs  e  -- ^ Result image.

  -- | Map an index aware function over each pixel in source image.
  imap :: Array arr cs' e' =>
          ((Int, Int) -> Pixel cs' e' -> Pixel cs e)
        -- ^ A function that takes an index @(i, j)@, a pixel at that location
        -- and returns a new pixel at the same location for the result image.
       -> Image arr cs' e' -- ^ Source image.
       -> Image arr cs e   -- ^ Result image.

  -- | Zip two images with a function
  zipWith :: (Array arr cs1 e1, Array arr cs2 e2) =>
             (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
          -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  -- | Zip two images with an index aware function
  izipWith :: (Array arr cs1 e1, Array arr cs2 e2) =>
              ((Int, Int) -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
           -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  -- | Traverse an image
  traverse :: Array arr cs' e' =>
              Image arr cs' e'
           -> ((Int, Int) -> (Int, Int))
           -> (((Int, Int) -> Pixel cs' e') ->
               (Int, Int) -> Pixel cs e)
           -> Image arr cs e
  
  -- | Traverse two images
  traverse2 :: (Array arr cs1 e1, Array arr cs2 e2) =>
               Image arr cs1 e1
            -> Image arr cs2 e2
            -> ((Int, Int) -> (Int, Int) -> (Int, Int))
            -> (((Int, Int) -> Pixel cs1 e1) ->
                ((Int, Int) -> Pixel cs2 e2) ->
                (Int, Int) -> Pixel cs e)
            -> Image arr cs e
  
  -- | Transpose an image
  transpose :: Image arr cs e -> Image arr cs e

  -- | Backwards permutation of an image
  backpermute :: (Int, Int) -- ^ New dimensions of an image
              -> ((Int, Int) -> (Int, Int)) -- ^ Function mapping index of each
                                            -- pixel to a new location.
              -> Image arr cs e -- ^ Source image
              -> Image arr cs e -- ^ Result image

  -- | Construct an image from nested rectangular shaped list of pixels.
  --
  -- >>> fromLists [[PixelY (fromIntegral (i*j) / 60000) | j <- [0..300]] | i <- [0..200]] :: Image RS Y Double
  -- >>>
  fromLists :: [[Pixel cs e]]
            -> Image arr cs e


class Array arr cs e => ManifestArray arr cs e where

  -- | Get a pixel at @i@-th and @j@-th location.
  --
  -- >>> let grad_gray = computeS $ makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
  -- >>> index grad_gray (20, 30) == PixelY ((20*30) / (200*200))
  -- True
  --
  index :: Image arr cs e -> (Int, Int) -> Pixel cs e

  deepSeqImage :: Image arr cs e -> a -> a
  
  (|*|) :: Image arr cs e -> Image arr cs e -> Image arr cs e

  fold :: (Pixel cs e -> Pixel cs e -> Pixel cs e)
       -> Pixel cs e -> Image arr cs e -> Pixel cs e

  eq :: Eq (Pixel cs e) => Image arr cs e -> Image arr cs e -> Bool


class ManifestArray arr cs e => MutableArray arr cs e where
  data MImage st arr cs e

  thaw :: PrimMonad m => Image arr cs e -> m (MImage (PrimState m) arr cs e)

  freeze :: PrimMonad m => MImage (PrimState m) arr cs e -> m (Image arr cs e)

  newImage :: PrimMonad m => (Int, Int) -> m (MImage (PrimState m) arr cs e)

  read :: PrimMonad m => MImage (PrimState m) arr cs e -> (Int, Int) -> m (Pixel cs e)

  write :: PrimMonad m => MImage (PrimState m) arr cs e -> (Int, Int) -> Pixel cs e -> m ()

  swap :: PrimMonad m => MImage (PrimState m) arr cs e -> (Int, Int) -> (Int, Int) -> m ()


  
  

class Transformable arr' arr where

  transform :: (Array arr' cs e, Array arr cs e) => arr -> Image arr' cs e -> Image arr cs e


-- | Transformation on the same type of array representation is disabled and 'transform'
-- will behave as identitity function.
instance Transformable arr arr where

  transform _ = id
  {-# INLINE transform #-}


instance (ColorSpace cs, Num e) => Num (Pixel cs e) where
  (+)         = pxOp2 (+)
  {-# INLINE (+) #-}
  
  (-)         = pxOp2 (-)
  {-# INLINE (-) #-}
  
  (*)         = pxOp2 (*)
  {-# INLINE (*) #-}
  
  abs         = pxOp abs
  {-# INLINE abs #-}
  
  signum      = pxOp signum
  {-# INLINE signum #-}
  
  fromInteger = fromChannel . fromInteger
  {-# INLINE fromInteger#-}
  

instance (ColorSpace cs, Fractional e) => Fractional (Pixel cs e) where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}
  
  recip        = pxOp recip
  {-# INLINE recip #-}

  fromRational = fromChannel . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs, Floating e) => Floating (Pixel cs e) where
  pi      = fromChannel pi
  {-# INLINE pi #-}

  exp     = pxOp exp
  {-# INLINE exp #-}

  log     = pxOp log
  {-# INLINE log #-}
  
  sin     = pxOp sin
  {-# INLINE sin #-}
  
  cos     = pxOp cos
  {-# INLINE cos #-}
  
  asin    = pxOp asin
  {-# INLINE asin #-}
  
  atan    = pxOp atan
  {-# INLINE atan #-}
  
  acos    = pxOp acos
  {-# INLINE acos #-}
  
  sinh    = pxOp sinh
  {-# INLINE sinh #-}
  
  cosh    = pxOp cosh
  {-# INLINE cosh #-}
  
  asinh   = pxOp asinh
  {-# INLINE asinh #-}
  
  atanh   = pxOp atanh
  {-# INLINE atanh #-}
  
  acosh   = pxOp acosh
  {-# INLINE acosh #-}


instance (ManifestArray arr cs e, Eq (Pixel cs e)) => Eq (Image arr cs e) where
  (==) = eq
  {-# INLINE (==) #-}

  
instance Array arr cs e => Num (Image arr cs e) where
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
  
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger#-}


instance (Fractional (Pixel cs e), Fractional e, Array arr cs e) =>
         Fractional (Image arr cs e) where
  (/)          = zipWith (/)
  {-# INLINE (/) #-}
  
  fromRational = singleton . fromRational 
  {-# INLINE fromRational #-}


instance (Floating (Pixel cs e), Floating e, Array arr cs e) =>
         Floating (Image arr cs e) where
  pi    = singleton pi
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


instance Array arr cs e => Show (Image arr cs e) where
  show ((dims -> (m, n)) :: Image arr cs e) =
    "<Image "++(show (undefined :: arr))++" "++(show (undefined :: cs))++
    ": "++(show m)++"x"++(show n)++">"

