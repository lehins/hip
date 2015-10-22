-- |
-- Module      : Graphics.Image.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2015
-- License     : MIT
--
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell Image Processing (HIP) library. This implementation uses Unboxed
-- 'Vector's from <http://hackage.haskell.org/package/vector Vector> package as
-- an underlying representation for images.

module Graphics.Image.Unboxed (
  -- * Image
  --
  -- Images can be created using 'make' or by reading them from a file.
  --
  Image,
  -- * Creation
  make,
  -- * Accessors
  -- ** Dimensions
  rows, cols, dims,
  -- ** Indexing
  index, maybeIndex, defaultIndex, unsafeIndex,
  -- ** Interpolation
  Interpolation(..), interpolate,
  -- * Processing
  -- ** Pointwise
  map, imap, zipWith,
  -- ** Geometric
  transpose, backpermute,
  traverse, traverse2, traverse3,
  --unsafeTraverse, unsafeTraverse2, unsafeTraverse3,
  --flipH, flipV,
  -- ** Traversal
  -- * Composition
  --leftToRight, topToBottom,
  -- ** Geometric
  scale,
  -- resize, rotate, rotate',
  -- * Conversion
  fromVector, toVector,
  fromLists, toLists,
  -- * Input/Output
  -- $io
  -- ** Reading image files
  readImageGray, readImageRGB, readImageGrayA, readImageRGBA,
  -- ** Writing or displaying images
  writeImage,
  IO.SaveOption(..), IO.OutputFormat(..), IO.Saveable(..),
  displayImage, IO.setDisplayProgram
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Unboxed.Internal (Image, VectorStrategy(..), fromVector, toVector)
import Graphics.Image.Interface.Interpolation
import qualified Graphics.Image.Interface as I
import qualified Graphics.Image.Interface.Processing as I
import qualified Graphics.Image.Interface.IO as IO
import Graphics.Image.Unboxed.Pixel


--------------------------------------------------------------------------------
---- IO ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- $io HIP uses <http://hackage.haskell.org/package/JuicyPixels JuicyPixels> and
-- <http://hackage.haskell.org/package/netpbm Netpbm> to encode and decode
-- image files of different formats.

-- | Read any supported image file as a grayscale image.
readImageGray :: FilePath -> IO (Image Gray)
readImageGray = IO.readImage


-- | Read any supported image file as a color image in RGB colorspace.
readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB = IO.readImage

-- | Same as 'readGrayImage', but reads it in with an alpha channel. If an image
-- file doesn't have an alpha channel, it will be added with opacity set to
-- '1.0'.
readImageGrayA :: FilePath -> IO (Image (Alpha RGB))
readImageGrayA = IO.readImage


-- | Same as 'readGrayRGB', but reads it in with an alpha channel. If an image
-- file doesn't have an alpha channel, it will be added with opacity set to
-- '1.0'.
readImageRGBA :: FilePath -> IO (Image (Alpha RGB))
readImageRGBA = IO.readImage


-- | Write an image to file. This function will try it's best to guess an image
-- format it should be saved in by looking at the file extension.
writeImage :: (IO.Saveable Image px, Pixel px) =>
              FilePath -- ^ Name of the file where image will be saved.
           -> Image px -- ^ Image to be saved.
           -> [IO.SaveOption Image px]
           -- ^ A list of 'IO.SaveOptions' that will specify the output format
           -- colorspace etc. They are optional, pass an empty list to use
           -- default options.
           -> IO ()
writeImage path img options = IO.writeImage Identity path img options


-- | Display an image using an external program, which you can specify using
-- 'setDisplayProgram'. This is function writes an image into a temporary file
-- in a system defined temporary folder and passes that file to an external
-- viewer. This is also a blocking function, which means, running of the program
-- will be suspended until external viewer is closed.
displayImage :: (IO.Saveable Image px, Pixel px) =>
                Image px -- ^ Image to be displayed.
             -> IO ()
displayImage = IO.displayImage Identity

--------------------------------------------------------------------------------
---- Accessing and creeating ---------------------------------------------------
--------------------------------------------------------------------------------

-- | Create an image. Pixel type will determine what kind of image it will be.
make :: Pixel px =>
        Int                -- ^ @m@ rows
     -> Int                -- ^ @n@ columns
     -> (Int -> Int -> px) -- ^ function that takes @i@-th row and @j-th column
                           -- as an argument and returns a pixel for taht
                           -- location.
     -> Image px
make = I.make


-- | Get dimensions of the image. 
dims :: Pixel px =>
        Image px   -- ^ Source image.
     -> (Int, Int) -- ^ (m rows, n columns)
dims = I.dims


-- | Get the number of rows in the image.
rows :: Pixel px => Image px -> Int
rows = I.rows


-- | Get the number of columns in the image.
cols :: Pixel px => Image px -> Int
cols = I.cols


-- | Get a pixel at @i@-th and @j@-th location.
index :: Pixel px =>
         Image px -- ^ Source image
         -> Int   -- ^ @i@-th row
         -> Int   -- ^ @j@-th column
         -> px
index = I.index


-- | Get a pixel at @i@-th and @j@-th location with a default pixel. If index is
-- out of bounds 'Nothing' is returned, @'Just' px@ otherwise.
maybeIndex :: Pixel px =>
              Image px -- ^ Source image.
           -> Int    -- ^ @i@th row
           -> Int    -- ^ @j@th column.
           -> Maybe px
maybeIndex = I.maybeIndex


-- | Get a pixel at @i@-th and @j@-th location with a default pixel. If index is
-- out of bounds, default pixel will be returned.
defaultIndex :: Pixel px =>
                px       -- ^ Default pixel
             -> Image px -- ^ Source image
             -> Int      -- ^ @i@th row
             -> Int      -- ^ @j@th column
             -> px
defaultIndex = I.defaultIndex

               
-- | Get a pixel at @i@-th and @j@-th location without bounds check. Potentially
-- dangerous operation, so use 'index' instead, until you are sure your code
-- works and then, in order to speed it up, you can switch it to 'unsafeIndex'.
unsafeIndex :: Pixel px =>
               Image px -- ^ Source image
            -> Int      -- ^ @i@-th row
            -> Int      -- ^ @j@-th column
            -> px
unsafeIndex = I.unsafeIndex

-- | Retreive an interpolated pixel at @i@-th and @j@-th location.
interpolate :: (RealFrac (Inner px), Pixel px) =>
               Interpolation  -- ^ 'Interpolation' algorithm
            -> px             -- ^ Default pixel to be used when out of bounds.
            -> Image px       -- ^ Source image
            -> (Inner px)     -- ^ approximation of @i@ row
            -> (Inner px)     -- ^ approximation of @j@ column
            -> px
interpolate alg defPx img@(dims -> (m, n)) i j =
  I.interpolate alg defPx m n (unsafeIndex img) i j
  

--------------------------------------------------------------------------------
---- Processing ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Map a function over an image.
map :: (Pixel px1, Pixel px) =>
       (px1 -> px) -- ^ A function that takes a pixel of a source image and
                   -- returns a pixel for the new image a the same location
    -> Image px1   -- ^ Source image
    -> Image px
map = I.map


-- | Map an index aware function over an image.
imap :: (Pixel px1, Pixel px) =>
        (Int -> Int -> px1 -> px) 
        -- ^ A function that takes a pixel and it's @i@ and @j@ index of a
        -- source image and returns a pixel for the new image a the same
        -- location
     -> Image px1 -- ^ Source image
     -> Image px
imap = I.imap


-- | Zip two Images with a function.
zipWith :: (Pixel px1, Pixel px2, Pixel px) =>
           (px1 -> px2 -> px) 
           -- ^ Function that takes pixeels from both source images and returns
           -- a pixel for the new image at the same location
        -> Image px1 -- ^ First source image
        -> Image px2 -- ^ Second source image
        -> Image px
zipWith = I.zipWith


--- Geometric

-- | Transpose an image.
--
-- >>> displayImage lena
-- >>> displayImage $ transpose lena
-- 
transpose :: Pixel px =>
             Image px -- ^ Source image
          -> Image px
transpose = I.transpose          


-- | Backpermute an Image.
backpermute :: Pixel px =>
               Int      -- ^ @m@ rows and
            -> Int      -- ^ @n@ columns in a new image.
            -> (Int -> Int -> (Int, Int)) 
            -- ^ Function that maps each @i@-th and @j@-th location from source
            -- image to the new location of created image.
            -> Image px -- ^ Source image
            -> Image px
backpermute = I.backpermute


-- | Traverse an image.
traverse :: (Pixel px1, Pixel px) =>
            Image px1 -- ^ Source image
         -> (Int -> Int -> (Int, Int)) 
         -- ^ Function that takes values for @m@ rows and @n@ columns of a
         -- source image and returns dimensions of a new image.
         -> ((Int -> Int -> px1) -> Int -> Int -> px)
         -- ^ Function that takes an indexing function of a source image, that
         -- can be used to retreive pixels at any location of a source image,
         -- it also takes values for @i@-th row and @j@-th column of a new image
         -- and returns a new pixel for this location.
         -> Image px
traverse = I.traverse


-- | Traverse two images.
traverse2 :: (Pixel px1, Pixel px2, Pixel px) =>
             Image px1 -- ^ First source image
          -> Image px2 -- ^ Second source image
          -> (Int -> Int -> Int -> Int -> (Int, Int))
          -- ^ A function that takes @m1@, @n1@, @m2@, @n2@ dimensions for both
          -- source images and returns dimensions for new image
          -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> Int -> Int -> px)
          -- ^ function that takes indexing functions for both images, @i@ and
          -- @j@ values of the location in new image and returns the pixel for
          -- this new location.
          -> Image px
traverse2 = I.traverse2


-- | Traverse three images. Same as with 'traverse' and 'traverse2', but with
-- three images.
traverse3 :: (Pixel px1, Pixel px2, Pixel px3, Pixel px) =>
             Image px1 -- ^ First source image
          -> Image px2 -- ^ Second source image
          -> Image px3 -- ^ Third source image
          -> (Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int))
          -- ^ Function that takes dimensions for source images and return
          -- dimesnions for new image
          -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> 
              (Int -> Int -> px3) -> Int -> Int -> px)
          -- ^ Function that takes indexing functions for all three source
          -- images, @i@ and @j@ and returns new pixel.
          -> Image px
traverse3 = I.traverse3


-- | Scale an image by a factor while using 'Interpolation'.
--
-- >>> lena <- readImageRGB "lena.png"
-- >>> lena
-- <Image RGB: 128x128>
-- >>> scale Bilinear 0.5 lena
-- <Image RGB: 64x64>
-- >>> scale Bilinear 2 lena
-- <Image RGB: 1024x1024>
--
scale :: (Pixel px, RealFrac (Inner px)) =>
         Interpolation -- ^ 'Interpolation' algorithm to be used during scaling.
      -> Inner px      -- ^ Scaling factor, must be greater than 0.
      -> Image px      -- ^ Image to be scaled.
      -> Image px
scale = I.scale

--------------------------------------------------------------------------------
---- Conversion ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Convert an image into a nested list of pixel. Outer layer will be of length
-- @m@ and inner all lists will be of length @n@.
toLists :: Pixel px => Image px -> [[px]]
toLists = I.toLists Identity


-- | Convert double nested lists into a two dimensional image. Length of an
-- outer list will constitute @m@ rows and length of inner lists - @n@
-- columns. All inner lists have to be the same length greater than 0.
fromLists :: Pixel px => [[px]] -> Image px
fromLists = I.fromLists
