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

module Graphics.Image (
  -- * Image
  Image,
  -- * Creation
  make,
  -- * Accessors
  -- ** Dimensions
  rows, cols, dims,
  -- ** Indexing
  index, maybeIndex, defaultIndex, unsafeIndex,
  -- ** Interpolation
  I.Method(..), interpolate, 
  -- * Processing
  -- ** Pointwise
  map, imap, zipWith,
  -- ** Geometric
  transpose, backpermute,
  traverse, traverse2, traverse3,
  unsafeTraverse, unsafeTraverse2, unsafeTraverse3,
  crop, pad,
  flipH, flipV,
  downsampleRows, downsampleCols, downsample,
  upsampleRows, upsampleCols, upsample,
  leftToRight, topToBottom,
  scale, resize,
  rotate, rotate',
  -- * Reduction
  maximum, minimum,
  -- * Binary
  module Graphics.Image.Binary,
  -- * Conversion
  toGrayImage, toRGBImage, toHSIImage,
  toAlphaImage, fromAlphaImage,
  toComplexImage, fromComplexImage,
  graysToRGB, graysToHSI, rgbToGrays, hsiToGrays,
  fromVector, toVector,
  fromLists, toLists,
  -- * Input/Output
  -- $io
  -- ** Reading image files
  readImageGray, readImageRGB, readImageGrayA, readImageRGBA,
  -- ** Writing or displaying images
  writeImage,
  displayImage, IO.setDisplayProgram,
  IO.SaveOption(..), IO.OutputFormat(..), IO.Saveable(..)
  ) where

import Prelude hiding (map, zipWith, maximum, minimum)
import Graphics.Image.Internal (Image, VectorStrategy(..), fromVector, toVector)
import qualified HIP.Interface as I
import qualified HIP.Conversion as C
import qualified HIP.Interpolation as I
import qualified HIP.Processing as I
import qualified HIP.IO as IO
import Graphics.Image.Binary
import Graphics.Image.Pixel


--------------------------------------------------------------------------------
---- IO ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- $io HIP uses <http://hackage.haskell.org/package/JuicyPixels JuicyPixels> and
-- <http://hackage.haskell.org/package/netpbm Netpbm> to encode and decode
-- image files of different formats.

-- | Read any supported image file as a grayscale image.
readImageGray :: FilePath -> IO (Image Gray)
readImageGray = IO.readImage


-- | Same as 'readGrayImage', but reads it in with an alpha channel. If an image
-- file doesn't have an alpha channel, it will be added with opacity set to
-- @1.0@.
readImageGrayA :: FilePath -> IO (Image (Alpha Gray))
readImageGrayA = IO.readImage


-- | Read any supported image file as a color image in RGB colorspace.
readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB = IO.readImage


-- | Same as 'readGrayRGB', but reads it in with an alpha channel. If an image
-- file doesn't have an alpha channel, it will be added with opacity set to
-- @1.0@.
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
-- 'setDisplayProgram'. By default 'gpicview' program is used. This is function
-- writes an image into a temporary file in a system defined temporary folder
-- and passes that file to an external viewer. This is also a blocking function,
-- which means, running of the program will be suspended until external viewer
-- is closed.
--
--  >>> frog <- readImageRGB "images/frog.jpg"
--  >>> displayImage frog
--
displayImage :: (IO.Saveable Image px, Pixel px) =>
                Image px -- ^ Image to be displayed.
             -> IO ()
displayImage = IO.displayImage Identity

--------------------------------------------------------------------------------
---- Accessing and creeating ---------------------------------------------------
--------------------------------------------------------------------------------

-- | Create an image. Pixel type will determine what kind of image it will be.
-- Notice, that in order to properly write an image to file, pixel values have
-- to be between 0 and 1.
--
-- >>> let grad_gray = make 200 200 (\i j -> Gray (fromIntegral i)/200 * (fromIntegral j)/200)
--
-- which is equivalent to:
--
-- >>> let grad_gray = (make 200 200 (\i j -> Gray $ fromIntegral (i*j))) / (200*200)
--
-- By default 'PNG' images saved 'inRGB8' colorspace, but we can specify
-- grayscale by supplying 'Encoder' 'inY8' as one of the 'SaveOption's.
--
-- >>> writeImage "images/grad_gray.png" grad_gray [Encoder inY8]
--
-- Creating color images is just as easy.
--
-- >>> let grad_color = make 200 200 (\i j -> RGB ((fromIntegral i)/200) ((fromIntegral j)/200) ((fromIntegral (i + j)) / 400))
-- >>> writeImage "images/grad_color.png" grad_color [Encoder inRGB16]
--
-- <<images/grad_gray.png>> <<images/grad_color.png>>
--
make :: Pixel px =>
        Int                -- ^ @m@ rows
     -> Int                -- ^ @n@ columns
     -> (Int -> Int -> px) -- ^ function that takes @i@-th row and @j-th column
                           -- as an argument and returns a pixel for taht
                           -- location.
     -> Image px
make = I.make


-- | Get the number of rows in the image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> rows frog
-- 200
--
rows :: Pixel px => Image px -> Int
rows = I.rows


-- | Get the number of columns in the image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> cols frog
-- 320
--
cols :: Pixel px => Image px -> Int
cols = I.cols


-- | Get dimensions of the image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> dims frog
-- (200,320)
--
dims :: Pixel px =>
        Image px   -- ^ Source image.
     -> (Int, Int) -- ^ (m rows, n columns)
dims = I.dims


-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> let grad_gray = make 200 200 (\i j -> Gray $ fromIntegral (i*j)) / (200*200)
-- >>> index grad_gray 20 30 == Gray (fromIntegral (20*30) / (200*200))
-- True
--
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
           -> Int    -- ^ @i@-th row
           -> Int    -- ^ @j@-th column.
           -> Maybe px
maybeIndex = I.maybeIndex


-- | Get a pixel at @i@-th and @j@-th location with a default pixel. If index is
-- out of bounds, default pixel will be returned.
defaultIndex :: Pixel px =>
                px       -- ^ Default pixel
             -> Image px -- ^ Source image
             -> Int      -- ^ @i@-th row
             -> Int      -- ^ @j@-th column
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
interpolate :: (I.Interpolation method px, RealFrac (Inner px), Pixel px) =>
               method     -- ^ One of the interpolation 'I.Method's.
            -> Image px   -- ^ Source image
            -> (Inner px) -- ^ approximation of @i@-th row
            -> (Inner px) -- ^ approximation of @j@-th column
            -> px
interpolate alg img@(dims -> (m, n)) i j =
  I.interpolate alg m n (unsafeIndex img) i j
  

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


-- | Traverse an image. Same as 'traverse', but without bounds check.
unsafeTraverse :: (Pixel px1, Pixel px) =>
            Image px1
         -> (Int -> Int -> (Int, Int)) 
         -> ((Int -> Int -> px1) -> Int -> Int -> px)
         -> Image px
unsafeTraverse = I.unsafeTraverse


-- | Traverse two images. Same as 'traverse2', but without bounds check.
unsafeTraverse2 :: (Pixel px1, Pixel px2, Pixel px) =>
                   Image px1
                -> Image px2
                -> (Int -> Int -> Int -> Int -> (Int, Int))
                -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> Int -> Int -> px)
                -> Image px
unsafeTraverse2 = I.unsafeTraverse2


-- | Traverse three images. Same as 'traverse3', but without bounds check.
unsafeTraverse3 :: (Pixel px1, Pixel px2, Pixel px3, Pixel px) =>
                   Image px1
                -> Image px2
                -> Image px3
                -> (Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int))
                -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> 
                    (Int -> Int -> px3) -> Int -> Int -> px)
                -> Image px
unsafeTraverse3 = I.unsafeTraverse3


-- | Crop an image, i.e. retrieves a sub-image image with @m@ rows and @n@
-- columns from source image starting at @i@-th and @j@-th pixel. Make sure
-- @(m + i, n + j)@ is not greater than dimensions of a source image.
--
-- >>> frog <- readImageGray "images/frog.jpg"
-- >>> frog
-- <Image Gray: 200x320>
-- >>> crop 20 60 100 200 frog
-- <Image Gray: 100x200>
-- >>> writeImage "images/frog_gray.jpg" frog []
-- >>> writeImage "images/frog_crop.jpg" (crop 20 60 100 200 frog) []
--
-- <<images/frog_gray.jpg>> <<images/frog_crop.jpg>>
--
crop :: Pixel px =>
        Int       -- ^ @i@ and 
     -> Int       -- ^ @j@ starting index from within an old image.
     -> Int       -- ^ @m@ rows and
     -> Int       -- ^ @n@ columns - dimensions of a new image.
     -> Image px  -- ^ Source image.
     -> Image px
crop = I.crop


-- | Changes dimensions of an image while padding it with a default pixel
-- whenever @i@ and @j@ is out of bounds for a source image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> pad (RGB 1 0 0) 250 150 frog
-- <Image RGB: 200x150>
-- >>> writeImage "images/frog_pad.jpg" (pad (RGB 1 0 0) 250 150 frog) []
--
-- <<images/frog.jpg>> <<images/frog_pad.jpg>>
--
pad :: Pixel px =>
       px        -- ^ default pixel to be used for out of bounds region
    -> Int       -- ^ @m@ rows
    -> Int       -- ^ @n@ columns
    -> Image px  -- ^ source image.
    -> Image px
pad = I.pad


-- | Flip an image horizontally.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_flipH.jpg" (flipH frog) []
--
-- <<images/frog.jpg>> <<images/frog_flipH.jpg>>
--
flipH :: Pixel px => Image px -> Image px
flipH = I.flipH


-- | Flip an image vertically.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_flipV.jpg" (flipV frog) []
--
-- <<images/frog.jpg>> <<images/frog_flipV.jpg>>
--
flipV :: Pixel px => Image px -> Image px
flipV = I.flipV


-- | Concatenate two images together one next to another into one. Both input
-- images must have the same number of rows.
leftToRight :: Pixel px =>
               Image px -- ^ First source image, that will be on the left.
            -> Image px -- ^ Second source image, that will be on the right.
            -> Image px
leftToRight = I.leftToRight


-- | Concatenate two images together one above another into one. Both input
-- images must have the same number of columns.
topToBottom :: Pixel px =>
               Image px -- ^ First source image, that will be on the top.
            -> Image px -- ^ Second source image, that will be on the bottom.
            -> Image px
topToBottom = I.topToBottom


-- | Downsample an image by discarding every odd row.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> downsampleRows frog
-- <Image RGB: 100x320>
-- >>> writeImage "images/frog_downsampleRows.jpg" (downsampleRows frog) []
--
-- <<images/frog.jpg>> <<images/frog_downsampleRows.jpg>>
--
downsampleRows :: Pixel px => Image px -> Image px
downsampleRows = I.downsampleRows


-- | Downsample an image by discarding every odd column.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> downsampleCols frog
-- <Image RGB: 200x160>
-- >>> writeImage "images/frog_downsampleCols.jpg" (downsampleCols frog) []
--
-- <<images/frog.jpg>> <<images/frog_downsampleCols.jpg>>
--
downsampleCols :: Pixel px => Image px -> Image px
downsampleCols = I.downsampleCols


-- | Downsample an image by discarding every odd row and column.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> downsample frog
-- <Image RGB: 100x160>
-- >>> writeImage "images/frog_downsample.jpg" (downsample frog) []
--
-- <<images/frog.jpg>> <<images/frog_downsample.jpg>>
--
downsample :: Pixel px => Image px -> Image px
downsample = I.downsample


-- | Upsample an image by inserting a row of back pixels after each row of a
-- source image.
--
-- >>> frog <- readImageRGB "images/frog_downsample.jpg"
-- >>> frog
-- <Image RGB: 100x160>
-- >>> upsampleRows frog
-- <Image RGB: 200x160>
-- >>> writeImage "images/frog_upsampleRows.jpg" (upsampleRows frog) []
--
-- <<images/frog_downsample.jpg>> <<images/frog_upsampleRows.jpg>>
--
upsampleRows :: Pixel px => Image px -> Image px
upsampleRows = I.upsampleRows


-- | Upsample an image by inserting a column of back pixels after each column of a
-- source image.
--
-- >>> frog <- readImageRGB "images/frog_downsample.jpg"
-- >>> frog
-- <Image RGB: 100x160>
-- >>> upsampleRows frog
-- <Image RGB: 100x320>
-- >>> writeImage "images/frog_upsampleCols.jpg" (upsampleCols frog) []
--
-- <<images/frog_downsample.jpg>> <<images/frog_upsampleCols.jpg>>
--
upsampleCols :: Pixel px => Image px -> Image px
upsampleCols = I.upsampleCols


-- | Upsample an image by inserting a row and a column of back pixels after each
-- row and a column of a source image.
--
-- >>> frog <- readImageRGB "images/frog_downsample.jpg"
-- >>> frog
-- <Image RGB: 100x160>
-- >>> upsampleRows frog
-- <Image RGB: 200x320>
-- >>> writeImage "images/frog_upsample.jpg" (upsample frog) []
--
-- <<images/frog_downsample.jpg>> <<images/frog_upsample.jpg>>
--
upsample :: Pixel px => Image px -> Image px
upsample = I.upsample

              
-- | Scale an image by a positive factor.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> scale (Bilinear 1) 0.5 frog
-- <Image RGB: 100x160>
-- >>> writeImage "images/frog_scale_nearest.jpg" (scale (Nearest 1) 1.5 frog) []
-- >>> writeImage "images/frog_scale_bilinear.jpg" (scale (Bilinear 1) 1.5 frog) []
-- 
-- <<images/frog_scale_nearest.jpg>> <<images/frog_scale_bilinear.jpg>>
--
scale :: (I.Interpolation method px, Pixel px, RealFrac (Inner px)) =>
         method    -- ^ One of the interpolation 'I.Method's to be used during
                   -- scaling.
      -> Inner px  -- ^ Scaling factor, must be greater than 0.
      -> Image px  -- ^ Image to be scaled.
      -> Image px
scale = I.scale


-- | Resize an image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> resize (Bilinear 1) 100 400 frog
-- <Image RGB: 100x400>
-- >>> writeImage "images/frog_resize.jpg" (resize (Bilinear 1) 100 400 frog) []
--
-- <<images/frog.jpg>> <<images/frog_resize.jpg>> 
--
resize :: (I.Interpolation method px, Pixel px, RealFrac (Inner px)) =>
         method   -- ^ One of the interpolation 'I.Method's to be used during
                  -- resizing.
      -> Int      -- ^ @m@ rows.
      -> Int      -- ^ @n@ columns.
      -> Image px -- ^ Source image to be resized.
      -> Image px
resize = I.resize


-- | Rotate an image around it's center by an angle Θ in counterclockwise
-- direction. Dimensions of a new image are adjusted in such a way that rotated
-- image fully fits inside.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate.jpg" (rotate (Bilinear (RGB 0 0.3 0)) (pi/6) frog) []
--
-- <<images/frog.jpg>> <<images/frog_rotate.jpg>> 
--
rotate :: (I.Interpolation method px, Pixel px, RealFloat (Inner px)) =>
          method   -- ^ One of the interpolation 'I.Method's to be used during
                   -- rotation.
       -> Inner px -- ^ Angle @theta@ in radians, that an image should be
                   -- rotated by.
       -> Image px -- ^ Source image to be rotated.
       -> Image px
rotate = I.rotate


-- | Rotate an image around it's center by an angle Θ in counterclockwise
-- direction. Dimensions of a new image will stay unchanged.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate'.jpg" (rotate' (Bilinear (RGB 0 0 0.5)) (pi/6) frog) []
--
-- <<images/frog.jpg>> <<images/frog_rotate'.jpg>> 
--
rotate' :: (I.Interpolation method px, Pixel px, RealFloat (Inner px)) =>
          method    -- ^ One of the interpolation 'I.Method's to be used during
                    -- rotation.
        -> Inner px -- ^ Angle @theta@ in radians, that an image should be
                    -- rotated by.
        -> Image px -- ^ Source image to be rotated.
        -> Image px
rotate' = I.rotate'

--------------------------------------------------------------------------------
---- Reduction -----------------------------------------------------------------
--------------------------------------------------------------------------------

maximum :: (Pixel px, Ord px) => Image px -> px
maximum = I.maximum Identity

minimum :: (Pixel px, Ord px) => Image px -> px
minimum = I.minimum Identity

--------------------------------------------------------------------------------
---- Conversion ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Converts an image to gray image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB: 200x320>
-- >>> toGrayImage frog
-- <Image Gray: 200x320>
-- 
toGrayImage :: (C.Convertible px Gray, Pixel px) =>
               Image px -- ^ Source image.
            -> Image Gray
toGrayImage = C.toGrayImage

-- | Convert an image to RGB image.
--
-- >>> toRGBImage $ toGrayImage frog
-- <Image RGB: 200x320>
-- 
toRGBImage :: (C.Convertible px RGB, Pixel px) =>
               Image px -- ^ Source image.
            -> Image RGB
toRGBImage = C.toRGBImage


-- | Convert an image to HSI image.
--
-- >>> toHSIImage frog
-- <Image HSI: 200x320>
-- 
toHSIImage :: (C.Convertible px HSI, Pixel px) =>
               Image px -- ^ Source image.
            -> Image HSI
toHSIImage = C.toHSIImage


-- |  Add an alpha channel to a regular image.
--
-- >>> toAlphaImage frog
-- <Image RGB-A: 200x320>
-- 
toAlphaImage :: (Pixel px, AlphaInner px) =>
                Image px -> Image (Alpha px)
toAlphaImage = C.toAlphaImage


-- |  Discard an alpha channel from an image.
--
-- >>> fromAlphaImage $ toAlphaImage frog
-- <Image RGB: 200x320>
-- 
fromAlphaImage :: (Pixel px, AlphaInner px) =>
                Image (Alpha px) -> Image px
fromAlphaImage = C.fromAlphaImage


-- | Convert a regular image to a Complex image by adding an all zero pixel imaginary
-- image.
toComplexImage :: (Pixel px, ComplexInner px) =>
                  Image px -> Image (Complex px)
toComplexImage = C.toComplexImage


-- | Convert Complex image to a regular image by dropping imaginary image.
fromComplexImage :: (Pixel px, ComplexInner px) =>
                    Image (Complex px) -> Image px
fromComplexImage = C.fromComplexImage


-- | Convert an RGB image to a three tuple of images containing (Red, Green, Blue)
-- values as 'Gray' values.
--
-- >>> let (red, green, blue) = rgbToGrays frog
-- writeImage "images/frog_red.png" red []
-- writeImage "images/frog_green.png" green []
-- writeImage "images/frog_blue.png" blue []
--
-- <<images/frog_red.png>> <<images/frog_green.png>> <<images/frog_blue.png>>
--
rgbToGrays :: Image RGB -> (Image Gray, Image Gray, Image Gray)
rgbToGrays = C.rgbToGrays


-- | Convert an HSI image to a three tuple of images containing (Hue, Saturation, Intensity)
-- values as 'Gray' values.
--
-- >>> let (hue, saturation, intensity) = hsiToGrays $ toHSIImage frog
-- >>> writeImage "images/frog_hue.png" ((hue + pi) / (2 * pi)) []
-- >>> writeImage "images/frog_saturation.png" saturation []
-- >>> writeImage "images/frog_intensity.png" intensity []
--
-- <<images/frog_hue.png>> <<images/frog_saturation.png>> <<images/frog_intensity.png>>
-- 
hsiToGrays :: Image HSI -> (Image Gray, Image Gray, Image Gray)
hsiToGrays = C.hsiToGrays


-- | Convert a three tuple of Gray images into an RGB image. All images must have
-- the same dimensions.
--
-- >>> graysToRGB $ rgbToGrays frog
-- <Image RGB: 200x320>
--
graysToRGB :: (Image Gray, Image Gray, Image Gray) -> Image RGB
graysToRGB = C.graysToRGB


-- | Convert a three tuple of Gray images into an HSI image. All images must have
-- the same dimensions.
--
-- >>> graysToHSI $ hsiToGrays $ toHSIImage frog
-- <Image HSI: 200x320>
--
graysToHSI :: (Image Gray, Image Gray, Image Gray) -> Image HSI
graysToHSI = C.graysToHSI


-- | Convert an image into a nested list of pixel. Outer layer will be of length
-- @m@ and inner all lists will be of length @n@.
--
-- >>> toLists $ make 2 3 (\i j -> Gray $ fromIntegral (i+j))
-- [[<Gray:(0.0)>,<Gray:(1.0)>,<Gray:(2.0)>],[<Gray:(1.0)>,<Gray:(2.0)>,<Gray:(3.0)>]]
--
toLists :: Pixel px => Image px -> [[px]]
toLists = I.toLists Identity


-- | Convert double nested lists into a two dimensional image. Length of an
-- outer list will constitute @m@ rows and length of inner lists - @n@
-- columns. All inner lists have to be the same length greater than 0.
--
-- >>> fromLists [[Gray $ fromIntegral (i*j) | j <- [0..300]] | i <- [0..200]] / 60000
-- <Image Gray: 200x300>
--
-- <<images/grad_fromLists.png>>
--
fromLists :: Pixel px => [[px]] -> Image px
fromLists = I.fromLists


