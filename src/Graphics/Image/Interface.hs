{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Graphics.Image.Interface (
  ColorSpace(..), Alpha(..), Array(..), ManifestArray(..), MutableArray(..), 
  Transformable(..)
  ) where

import Prelude hiding (map, zipWith, sum, product)
import GHC.Exts (Constraint)
import Control.Monad.Primitive (PrimMonad (..))
import Data.Typeable


class (ColorSpace (Opaque cs), ColorSpace cs) => Alpha cs where
  -- | An opaque version of this color space.
  type Opaque cs

  -- | Get an alpha channel of a transparant pixel. 
  getAlpha :: Pixel cs e -> e

  -- | Add an alpha channel of an opaque pixel.
  --
  -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e
  
  -- | Convert a transparent pixel to an opaque one by dropping the alpha
  -- channel.
  --
  -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
  --
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


class (Eq cs, Enum cs, Show cs) => ColorSpace cs where
  -- | Representation of a pixel, such that it can be an element of any Array
  type PixelElt cs e

  -- | A concrete Pixel representation for a particular color space.
  data Pixel cs e

  -- | Construt a pixel by replicating a same value among all of the channels.
  fromChannel :: e -> Pixel cs e

  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toElt :: Pixel cs e -> PixelElt cs e

  -- | Convert from an elemnt representation back to a Pixel.
  fromElt :: PixelElt cs e -> Pixel cs e

  -- | Retrieve Pixel's channel value
  getPxCh :: Pixel cs e -> cs -> e
  
  -- | Map a function over all Pixel's channels.
  pxOp :: (e' -> e) -> Pixel cs e' -> Pixel cs e

  -- | Zip two Pixels with a function.
  pxOp2 :: (e1 -> e2 -> e) -> Pixel cs e1 -> Pixel cs e2 -> Pixel cs e
  
  -- | Map a channel aware function over all Pixel's channels.
  chOp :: (cs -> e' -> e) -> Pixel cs e' -> Pixel cs e 

  -- | Zip two Pixels with a channel aware function.
  chOp2 :: (cs -> e1 -> e2 -> e) -> Pixel cs e1 -> Pixel cs e2 -> Pixel cs e
                                 
  -- | Apply a function to a specified channel of a Pixel.
  chApp :: (e -> e) -> cs -> Pixel cs e -> Pixel cs e
  chApp !f !ch = chOp (\ !ch' !e -> if ch' == ch then f e else e)
  
class (Show arr, ColorSpace cs, Num (Pixel cs e), Num e, Typeable e, Elt arr cs e) =>
      Array arr cs e where
  
  type Elt arr cs e :: Constraint
  type Elt arr cs e = ()
  data Image arr cs e

  -- | Create an Image by supplying it's dimensions and a pixel genrating
  -- function.
  makeImage :: (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
            -> ((Int, Int) -> Pixel cs e)
               -- ^ A function that takes (@i@-th row, and @j@-th column) as an
               -- argument and returns a pixel for that location.
            -> Image arr cs e

  -- | Create a singleton image, required for various operations on images with
  -- a scalar.
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

  -- | Map a function over a an image.
  map :: Array arr cs' e' =>
         (Pixel cs' e' -> Pixel cs e)
         -- ^ A function that takes a pixel of a source image and returns a pixel
         -- for the result image a the same location.
      -> Image arr cs' e' -- ^ Source image.
      -> Image arr cs  e  -- ^ Result image.

  -- | Map an index aware function over each pixel in an image.
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
              Image arr cs' e' -- ^ Source image.
           -> ((Int, Int) -> (Int, Int))
           -- ^ Function that takes dimensions of a source image and returns
           -- dimensions of a new image.
           -> (((Int, Int) -> Pixel cs' e') ->
               (Int, Int) -> Pixel cs e)
           -- ^ Function that receives a pixel getter (a source image index
           -- function), a location @(i, j)@ in a new image and returns a pixel
           -- for that location.
           -> Image arr cs e
  
  -- | Traverse two images.
  traverse2 :: (Array arr cs1 e1, Array arr cs2 e2) =>
               Image arr cs1 e1 -- ^ First source image.
            -> Image arr cs2 e2 -- ^ Second source image.
            -> ((Int, Int) -> (Int, Int) -> (Int, Int))
            -- ^ Function that produces dimensions for the new image.
            -> (((Int, Int) -> Pixel cs1 e1) ->
                ((Int, Int) -> Pixel cs2 e2) ->
                (Int, Int) -> Pixel cs e)
            -- ^ Function that produces pixels for the new image.
            -> Image arr cs e
  
  -- | Transpose an image
  transpose :: Image arr cs e -> Image arr cs e

  -- | Backwards permutation of an image. 
  backpermute :: (Int, Int) -- ^ Dimensions of a result image.
              -> ((Int, Int) -> (Int, Int))
                 -- ^ Function that maps an index of a source image to an index
                 -- of a result image.
              -> Image arr cs e -- ^ Source image.
              -> Image arr cs e -- ^ Result image.

  -- | Construct an image from a nested rectangular shaped list of pixels.
  -- Length of an outer list will constitute @m@ rows, while the length of inner lists -
  -- @n@ columns. All of the inner lists must be the same length and greater than @0@.
  --
  -- >>> fromLists [[PixelY (fromIntegral (i*j) / 60000) | j <- [1..300]] | i <- [1..200]] :: Image VU Y Double
  -- <Image VectorUnboxed Luma: 200x300>
  --
  -- <<images/grad_fromLists.png>>
  --
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
  
  -- | Make sure that an image is fully evaluated.
  deepSeqImage :: Image arr cs e -> a -> a

  -- | Perform matrix multiplication on two images. Inner dimensions must agree.
  (|*|) :: Image arr cs e -> Image arr cs e -> Image arr cs e

  -- | Undirected reduction of an image.
  fold :: (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
       -> Pixel cs e -- ^ Initial element, that is neutral with respect to folding function.
       -> Image arr cs e -- ^ Source image.
       -> Pixel cs e

  -- ^ Pixelwise equality function of two images. If either dimensions of both images
  -- or at least one pair of corresponding pixels ar not the same, images are
  -- considered distinct. Used in defining an in instance for the 'Eq'
  -- typeclass.
  eq :: Eq (Pixel cs e) => Image arr cs e -> Image arr cs e -> Bool


class ManifestArray arr cs e => MutableArray arr cs e where
  data MImage st arr cs e

  mdims :: MImage st arr cs e -> (Int, Int)

  thaw :: PrimMonad m => Image arr cs e -> m (MImage (PrimState m) arr cs e)

  freeze :: PrimMonad m => MImage (PrimState m) arr cs e -> m (Image arr cs e)

  newImage :: PrimMonad m => (Int, Int) -> m (MImage (PrimState m) arr cs e)

  read :: PrimMonad m => MImage (PrimState m) arr cs e -> (Int, Int) -> m (Pixel cs e)

  write :: PrimMonad m => MImage (PrimState m) arr cs e -> (Int, Int) -> Pixel cs e -> m ()

  swap :: PrimMonad m => MImage (PrimState m) arr cs e -> (Int, Int) -> (Int, Int) -> m ()



class Transformable arr' arr where

  -- | Transform the underlying array representation of an image.
  transform :: (Array arr' cs e, Array arr cs e) =>
               arr -- ^ New representation of an image.
            -> Image arr' cs e -- ^ Source image.
            -> Image arr cs e


-- | Transformation to the same type of array representation is disabled and 'transform'
-- will behave simply as an identitity function.
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
    "<Image "++show (undefined :: arr)++" "++show (undefined :: cs)++" ("++
    ((showsTypeRep (typeOf (undefined :: e))) "): "++show m++"x"++show n++">")


instance MutableArray arr cs e => Show (MImage st arr cs e) where
  show ((mdims -> (m, n)) :: MImage st arr cs e) =
    "<MutableImage "++show (undefined :: arr)++" "++show (undefined :: cs)++" ("++
    ((showsTypeRep (typeOf (undefined :: e))) "): "++show m++"x"++show n++">")


