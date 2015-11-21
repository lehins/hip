{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Graphics.Image.Repa.Sequential (
  -- * Computation
  compute,
  -- * Initialization
  make,
  -- * Pixelwise Operations
  -- ** Mapping
  map, imap,
  -- ** Traversing
  traverse, traverse2, traverse3,
  -- ** Zipping
  zipWith,
  -- ** Permutations
  backpermute, transpose,
  -- * Processing
  -- ** Extracting
  crop, fold, sum, maximum, minimum, normalize,
  -- * Convolution
  convolve, convolveRows, convolveCols, convolve',
  -- * Fast Fourier Transform
  fft, ifft,
  -- * Conversion
  fromArray, toArray, fromVector, toVector, fromLists, toLists,
  -- * IO
  readImageGray, readImageGrayA, readImageRGB, readImageRGBA,
  IO.SaveOption(..),
  writeImage, displayImage
  ) where

import Prelude hiding (maximum, minimum, sum, map, zipWith)
import Data.Array.Repa (U, DIM2, Array)
import Data.Vector.Unboxed (Vector)
import Graphics.Image.Repa.Internal (Image, RepaStrategy(..))
import Graphics.Image.Repa.Pixel
import qualified Graphics.Image.Repa.Internal as I
import qualified HIP.IO as IO
import qualified HIP.Processing as P
import qualified HIP.Complex as C


make :: Pixel px =>
        Int
     -> Int
     -> (Int -> Int -> px)
     -> Image px
make !m !n = compute . I.make m n


{-| Map a function over an image. -}
map :: (Pixel px1, Pixel px) =>
       (px1 -> px)
    -> Image px1
    -> Image px
map !f = compute . I.map f


{-| Apply a function to every pixel of an image and its index. -}
imap :: (Pixel px1, Pixel px) =>
        (Int -> Int -> px1 -> px)
     -> Image px1
     -> Image px
imap !f = compute . I.imap f


-- | Zip two Images with a function.
zipWith :: (Pixel px1, Pixel px2, Pixel px) =>
           (px1 -> px2 -> px)
        -> Image px1
        -> Image px2
        -> Image px
zipWith !f !img = compute . I.zipWith f img


-- | Traverse an image.
traverse :: (Pixel px1, Pixel px) =>
            Image px1
         -> (Int -> Int -> (Int, Int))
         -> ((Int -> Int -> px1) -> Int -> Int -> px)
         -> Image px
traverse !img !f = compute . I.traverse img f


-- | Traverse two images.
traverse2 :: (Pixel px1, Pixel px2, Pixel px) =>
             Image px1
          -> Image px2
          -> (Int -> Int -> Int -> Int -> (Int, Int))
          -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> Int -> Int -> px)
          -> Image px
traverse2 !img1 !img2 !f = compute . I.traverse2 img1 img2 f


-- | Traverse three images.
traverse3 :: (Pixel px1, Pixel px2, Pixel px3, Pixel px) =>
             Image px1
          -> Image px2
          -> Image px3
          -> (Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int))
          -> (   (Int -> Int -> px1)
              -> (Int -> Int -> px2)
              -> (Int -> Int -> px3)
              -> Int -> Int -> px)
          -> Image px
traverse3 !img1 !img2 !img3 !f =
  compute . I.traverse3 (compute img1) (compute img2) (compute img3) f


-- | Transpose an image
transpose :: Pixel px =>
             Image px -- ^ Source image
          -> Image px
transpose = compute . I.transpose          


-- | Backpermute an Image
backpermute :: Pixel px =>
               Int                        -- ^ @m@ rows and
            -> Int                        -- ^ @n@ columns in a new image.
            -> (Int -> Int -> (Int, Int)) -- ^ Function that maps each location
                                          -- from source image to a new image to 
            -> Image px                   -- ^ Source image
            -> Image px
backpermute !m !n !f = compute . I.backpermute m n f


-- | Crop an image, i.e. retrieves a sub-image image with @m@ rows and @n@
-- columns. Make sure @(m + i, n + j)@ is not greater than dimensions of a
-- source image.
crop :: Pixel px =>
        Int       -- ^ @i@ and 
     -> Int       -- ^ @j@ starting index from within an old image.
     -> Int       -- ^ @m@ rows and
     -> Int       -- ^ @n@ columns. Dimensions of a new image @m@ and @n@.
     -> Image px  -- ^ Source image.
     -> Image px
crop !i !j !m !n = compute . I.crop i j m n 


compute :: Pixel px =>
           Image px
        -> Image px
compute = I.compute I.Sequential
{-# INLINE compute #-}


fold :: Pixel px =>
        (px -> px -> px)
     -> px
     -> Image px
     -> px
fold = I.fold Sequential 


sum :: Pixel px => Image px -> px
sum = I.sum Sequential


maximum :: (Pixel px, Ord px) =>
           Image px
        -> px
maximum = I.maximum I.Sequential


minimum :: (Pixel px, Ord px) =>
           Image px
        -> px
minimum = I.minimum Sequential


normalize :: (Pixel px, Fractional px, Ord px) =>
             Image px
          -> Image px
normalize = I.normalize Sequential


convolve :: Pixel px => Image px -> Image px -> Image px
convolve !krn !img = compute $ P.convolve P.Wrap (compute krn) (compute img)


convolveRows :: Pixel px => [px] -> Image px -> Image px
convolveRows !ls !img = convolve (I.transpose . I.fromLists $ [ls]) img


convolveCols :: Pixel px => [px] -> Image px -> Image px
convolveCols !ls !img = convolve (I.fromLists $ [ls]) img


convolve' :: Pixel px => P.Outside px -> Image px -> Image px -> Image px
convolve' outside !krn !img = compute $ P.convolve outside (compute krn) (compute img)


fft :: ComplexInner px => Image (Complex px) -> Image (Complex px)
fft = C.fft I.Sequential


ifft :: ComplexInner px =>
        Image (Complex px) -> Image (Complex px)
ifft = C.ifft I.Sequential


fromVector :: (Pixel px) =>
              Int
           -> Int
           -> Vector px
           -> Image px
fromVector !m !n = compute . I.fromVector m n


toVector :: (Pixel px) =>
            Image px
         -> Vector px
toVector = I.toVector I.Sequential


fromLists :: (Pixel px) =>
             [[px]]
          -> Image px
fromLists = compute . I.fromLists


toLists :: (Pixel px) =>
           Image px
        -> [[px]]
toLists = I.toLists I.Sequential


fromArray :: (Pixel px) =>
             Array U DIM2 px
          -> Image px
fromArray = compute . I.fromArray 


toArray :: (Pixel px) =>
           Image px
        -> Array U DIM2 px
toArray = I.toArray I.Sequential


-- IO

readImageGray :: FilePath -> IO (Image Gray)
readImageGray = fmap compute . IO.readImage


readImageGrayA :: FilePath -> IO (Image (Alpha Gray))
readImageGrayA = fmap compute . IO.readImage


readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB = fmap compute . IO.readImage


readImageRGBA :: FilePath -> IO (Image (Alpha RGB))
readImageRGBA = fmap compute . IO.readImage


writeImage :: (IO.Saveable Image px, Pixel px) =>
              FilePath
           -> Image px
           -> [IO.SaveOption Image px]
           -> IO ()
writeImage !path !img !options = IO.writeImage I.Sequential path img options


displayImage :: (IO.Saveable Image px, Pixel px) =>
           Image px
        -> IO ()
displayImage = IO.displayImage I.Sequential

