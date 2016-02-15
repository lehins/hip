{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : MIT
--
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell Image Processing (HIP) library. This implementation uses
-- <http://hackage.haskell.org/package/repa Repa> package as an underlying
-- representation of images and their processing in parallel or sequentially.
module Graphics.Image (
  -- * Pixels
  module Graphics.Image.ColorSpace,
  -- * Creation
  makeImage,
  -- * Computation
  R.computeS, R.computeP, transform,
  -- * IO
  R.readImageY, R.readImageYA, R.readImageRGB, R.readImageRGBA,
  writeImage,
  -- * Accessors
  -- ** Dimensions
  rows, cols, dims,
  -- ** Indexing
  index,
  -- * Processing
  -- ** Pointwise
  map, imap, zipWith, izipWith,
  -- ** Geometric
  (|*|), transpose, backpermute,
  traverse, traverse2,
  -- * Reduction
  fold, sum, product, --maximum, minimum,
  ) where
import Prelude hiding (map, zipWith, sum, product)
import Graphics.Image.ColorSpace
import Graphics.Image.IO --(writeImage, readImage)
import Graphics.Image.Interface (Array, ManifestArray, Image, ColorSpace(..))
import qualified Graphics.Image.Interface as I
import qualified Graphics.Image.Repa as R


--------------------------------------------------------------------------------
---- Creation and Transformation -----------------------------------------------
--------------------------------------------------------------------------------

-- | Create a delayed representation of image with 'Double' precision. If it is
-- required to create an image in different representation or with some other
-- precision, you can use 'I.makeImage' from <Graphics-Image-Interface.html Interface> module
-- while specifying it's output type. Note that with 'Double' precision it is
-- essential to keep pixel values normalized in the @[0, 1]@ range in order for
-- an image to be written to file properly.
--
-- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY (fromIntegral i)/200 * (fromIntegral j)/200)
--
-- which is equivalent to:
--
-- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> writeImage "images/grad_gray.png" $ computeS grad_gray
--
-- Creating color images is just as easy.
--
-- >>> let grad_color = makeImage (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400
-- >>> writeImage "images/grad_color.png" $ computeS grad_color
--
-- <<images/grad_gray.png>> <<images/grad_color.png>>
--
makeImage :: Array R.RD cs Double =>
             (Int, Int) -- ^ (@m@ rows, @n@ columns)
          -> ((Int, Int) -> Pixel cs Double)
             -- ^ A function that takes @i@-th row and @j-th column as an argument
             -- and returns a pixel for that location.
          -> Image R.RD cs Double
makeImage = I.makeImage
{-# INLINE makeImage #-}


transform :: (I.Transformable arr' arr, Array arr' cs e, Array arr cs e) =>
             Image arr' cs e
          -> arr
          -> Image arr cs e
transform = I.transform
{-# INLINE transform #-}


-- | Get dimensions of an image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RepaDelayed RGB: 200x320>
-- >>> dims frog
-- (200,320)
--
dims :: Array arr cs e =>
        Image arr cs e
     -> (Int, Int)
dims = I.dims     
{-# INLINE dims #-}


-- | Get the number of rows in an image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RepaDelayed RGB: 200x320>
-- >>> rows frog
-- 200
--
rows :: Array arr cs e => Image arr cs e -> Int
rows = fst . I.dims
{-# INLINE rows #-}


-- | Get the number of columns in an image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RepaDelayed RGB: 200x320>
-- >>> cols frog
-- 320
--
cols :: Array arr cs e => Image arr cs e -> Int
cols = snd . I.dims
{-# INLINE cols #-}



-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> let grad_gray = computeS $ makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> index grad_gray (20, 30) == PixelY ((20*30) / (200*200))
-- True
--
index :: ManifestArray arr cs e =>
         Image arr cs e
      -> (Int, Int)
      -> Pixel cs e
index = I.index
{-# INLINE index #-}


-- | Map a function overa a source image.
map :: (Array arr cs' e', Array arr cs e) =>
       (Pixel cs' e' -> Pixel cs e)
       -- ^ A function that takes a pixel of a source image and returns a pixel
       -- for the result image a the same location.
    -> Image arr cs' e' -- ^ Source image.
    -> Image arr cs e   -- ^ Result image.
map = I.map
{-# INLINE map #-}


-- | Map an index aware function over each pixel in source image.
imap :: (Array arr cs' e', Array arr cs e) =>
        ((Int, Int) -> Pixel cs' e' -> Pixel cs e)
        -- ^ A function that takes an index @(i, j)@, a pixel at that location
        -- and returns a new pixel at the same location for the result image.
        -> Image arr cs' e' -- ^ Source image.
        -> Image arr cs e   -- ^ Result image.
imap = I.imap
{-# INLINE imap #-}


zipWith :: (Array arr cs1 e1, Array arr cs2 e2, Array arr cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image arr cs1 e1
        -> Image arr cs2 e2
        -> Image arr cs e
zipWith = I.zipWith        
{-# INLINE zipWith #-}
        

izipWith :: (Array arr cs1 e1, Array arr cs2 e2, Array arr cs e) =>
            ((Int, Int) -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
         -> Image arr cs1 e1
         -> Image arr cs2 e2
         -> Image arr cs e
izipWith = I.izipWith
{-# INLINE izipWith  #-}
         

traverse :: (Array arr cs' e', Array arr cs e) =>
            Image arr cs' e'
         -> ((Int, Int) -> (Int, Int))
         -> (((Int, Int) -> Pixel cs' e') ->
             (Int, Int) -> Pixel cs e)
         -> Image arr cs e
traverse = I.traverse
{-# INLINE traverse #-}

           
traverse2 :: (Array arr cs1 e1, Array arr cs2 e2, Array arr cs e) =>
             Image arr cs1 e1
          -> Image arr cs2 e2
          -> ((Int, Int) -> (Int, Int) -> (Int, Int))
          -> (((Int, Int) -> Pixel cs1 e1) ->
              ((Int, Int) -> Pixel cs1 e1) ->
              (Int, Int) -> Pixel cs e)
          -> Image arr cs e
traverse2 = I.traverse2
{-# INLINE traverse2 #-}


(|*|) :: ManifestArray arr cs e =>
         Image arr cs e
      -> Image arr cs e
      -> Image arr cs e
(|*|) = (I.|*|)

            
transpose :: Array arr cs e =>
             Image arr cs e
          -> Image arr cs e
transpose = I.transpose
{-# INLINE transpose #-}


backpermute :: Array arr cs e =>
               (Int, Int)
            -> ((Int, Int) -> (Int, Int))
            -> Image arr cs e
            -> Image arr cs e
backpermute = I.backpermute
{-# INLINE backpermute #-}

  
fold :: ManifestArray arr cs e =>
        (Pixel cs e -> Pixel cs e -> Pixel cs e)
     -> Pixel cs e
     -> Image arr cs e
     -> Pixel cs e
fold = I.fold
{-# INLINE fold #-}

       
sum :: ManifestArray arr cs e =>
       Image arr cs e -> Pixel cs e
sum = I.sum
{-# INLINE sum #-}


product :: ManifestArray arr cs e =>
           Image arr cs e -> Pixel cs e
product = I.product
{-# INLINE product #-}
