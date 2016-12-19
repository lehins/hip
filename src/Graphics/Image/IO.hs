{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.IO
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO (
  -- * Reading
  readImage, readImageExact,
  -- * Writing
  writeImage, writeImageExact,
  -- * Displaying
  ExternalViewer(..),
  displayImage,
  displayImageUsing,
  displayImageFile,
  -- ** Common viewers
  defaultViewer,
  eogViewer,
  gpicviewViewer,
  fehViewer,
  gimpViewer,
  -- * Supported Image Formats
  module Graphics.Image.IO.Formats
  
  -- $supported
  
  ) where

import Prelude as P hiding (readFile, writeFile)
import qualified Control.Monad as M (foldM)

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString as B (readFile)
import Graphics.Image.ColorSpace
import Graphics.Image.Interface
import Graphics.Image.IO.Base
import Graphics.Image.IO.Formats
import qualified Data.ByteString.Lazy as BL (writeFile, hPut)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcess)
import Control.Exception (bracket)


-- | External viewing application to use for displaying images.
data ExternalViewer =
  ExternalViewer FilePath [String] Int
    -- ^ Any custom viewer, which can be specified:
    -- 
    -- * @FilePath@ - to the actual viewer executable.
    -- * @[String]@ - command line arguments that will be passed to the executable.
    -- * @Int@ - position index in the above list where `FilePath` to an image should be
    -- injected
  deriving Show


guessFormat :: (ImageFormat f, Enum f) => FilePath -> Maybe f
guessFormat path =
  headMaybe . dropWhile (not . isFormat e) . enumFrom . toEnum $ 0
  where e = P.map toLower . takeExtension $ path
        headMaybe ls = if null ls then Nothing else Just $ head ls

-- | This function will try to guess an image format from file's extension,
-- then it will attempt to read it as such. It will fall back onto the rest of
-- the supported formats and will try to read them regarless of file's
-- extension. Whenever image cannot be decoded, 'Left' containing all errors for
-- each attempted format will be returned, and 'Right' containing an image
-- otherwise. Image will be read into a type signature specified 'ColorSpace'
-- ('Graphics.Image.ColorSpace.Y', 'Graphics.Image.ColorSpace.YA',
-- 'Graphics.Image.ColorSpace.RGB' and 'Graphics.Image.ColorSpace.RGBA' only)
-- with 'Double' precision, while doing all necessary conversions.
readImage :: Readable (Image arr cs Double) InputFormat =>
             FilePath
          -> IO (Either String (Image arr cs Double))
readImage path = do
  imgstr <- B.readFile path
  let maybeFormat = guessFormat path :: Maybe InputFormat
      formats = enumFrom . toEnum $ 0
      orderedFormats = maybe formats (\f -> f:filter (/=f) formats) maybeFormat
      reader (Left err) format = 
        return $ either (Left . ((err++"\n")++)) Right (decode format imgstr)
      reader img         _     = return img
  M.foldM reader (Left "") orderedFormats

-- | This function allows for reading any supported image in the exact colorspace
-- and precision it is currently encoded in. For instance, frog image can be
-- read into it's 'Graphics.Image.ColorSpace.YCbCr' colorspace with
-- 'Graphics.Image.ColorSpace.Word8' precision and into any supported array
-- representation.
--
-- >>> readImageExact JPG "images/frog.jpg" :: IO (Either String (Image RP YCbCr Word8))
-- Right <Image RepaParallel YCbCr (Word8): 200x320>
--
-- The drawback here is that colorspace and precision has to match exactly,
-- otherwise it will return an error:
--
-- >>> readImageExact JPG "images/frog.jpg" :: IO (Either String (Image RP RGB Word8))
-- Left "JuicyPixel decoding error: Input image is in YCbCr8 (Pixel YCbCr Word8), cannot convert it to RGB8 (Pixel RGB Word8) colorspace."
--
-- Attempt to read an image in a particular color space that is not supported by
-- the format, will result in a compile error. Refer to 'Readable' class for all
-- images that can be decoded.
readImageExact :: Readable img format =>
                  format
                  -- ^ A file format that an image should be read as. See
                   -- <#g:4 Supported Image Formats>
               -> FilePath -- ^ Location of an image.
               -> IO (Either String img)
readImageExact format path = fmap (decode format) (B.readFile path)


-- | Just like 'readImage', this function will guess an output file format from the
-- extension and write to file any image that is in one of 'Y', 'YA', 'RGB' or
-- 'RGBA' color spaces with 'Double' precision. While doing necessary
-- conversions the choice will be given to the most suited color space supported
-- by the format. For instance, in case of a 'PNG' format, an ('Image' @arr@
-- 'RGBA' 'Double') would be written as @RGBA16@, hence preserving transparency
-- and using highest supported precision 'Word16'. At the same time, writing
-- that image in 'GIF' format would save it in @RGB8@, since 'Word8' is the
-- highest precision 'GIF' supports and it currently cannot be saved with
-- transparency.
writeImage :: Writable (Image arr cs e) OutputFormat =>
              FilePath            -- ^ Location where an image should be written.
           -> Image arr cs e -- ^ An image to write. 
           -> IO ()
writeImage path = BL.writeFile path . encode format [] where
  format = fromMaybe (error ("Could not guess output format. Use 'writeImageExact' "++
                         "or supply a filename with supported format."))
           (guessFormat path :: Maybe OutputFormat)


-- | Write an image in a specific format, while supplying any format specific
-- options. Precision and color space, that an image will be written as, is decided
-- from image's type. Attempt to write image file in a format that does not
-- support color space and precision combination will result in a compile error.
writeImageExact :: Writable img format =>
                   format
                   -- ^ A file format that an image should be saved in. See
                   -- <#g:4 Supported Image Formats>
                -> [SaveOption format] -- ^ A list of format specific options.
                -> FilePath -- ^ Location where an image should be written.
                -> img -- ^ An image to write. Can be a list of images in case
                       -- of formats supporting animation.
                -> IO ()
writeImageExact format opts path = BL.writeFile path . encode format opts
  

{- | An image is written as a @.tiff@ file into an operating system's temporary
directory and passed as an argument to the external viewer program. -}
displayImageUsing :: Writable (Image arr cs e) TIF =>
                     ExternalViewer -- ^ External viewer to use
                  -> Bool -- ^ Should the call be blocking
                  -> Image arr cs e -- ^ Image to display
                  -> IO ()
displayImageUsing viewer block img = do
  let display = do
        tmpDir <- fmap (</> "hip") getTemporaryDirectory
        createDirectoryIfMissing True tmpDir
        bracket (openBinaryTempFile tmpDir "tmp-img.tiff")
          (hClose . snd)
          (\ (imgPath, imgHandle) -> do
              BL.hPut imgHandle $ encode TIF [] img
              hClose imgHandle
              displayImageFile viewer imgPath)
  if block
    then display
    else void $ forkIO display



-- | Displays an image file by calling an external image viewer.
displayImageFile :: ExternalViewer -> FilePath -> IO ()
displayImageFile (ExternalViewer exe args ix) imgPath =
  void $ readProcess exe (argsBefore ++ [imgPath] ++ argsAfter) ""
  where (argsBefore, argsAfter) = splitAt ix args


{- | Makes a call to an external viewer that is set as a default image viewer by
the OS. This is a non-blocking function call, so it will take some time before
an image will appear.

  >>> frog <- readImageRGB VU "images/frog.jpg"
  >>> displayImage frog

-}
displayImage :: Writable (Image arr cs e) TIF =>
                Image arr cs e -- ^ Image to be displayed
             -> IO ()
displayImage = displayImageUsing defaultViewer False


defaultViewer :: ExternalViewer
defaultViewer =
#if defined(OS_Win32)
  (ExternalViewer "explorer.exe" [] 0)
#elif defined(OS_Linux)
  (ExternalViewer "xdg-open" [] 0)
#elif defined(OS_Mac)
  (ExternalViewer "open" [] 0)
#else
  error "Graphics.Image.IO.defaultViewer: Could not determine default viewer."
#endif


-- | @eog /tmp/hip/img.tiff@
-- <https://help.gnome.org/users/eog/stable/ Eye of GNOME>
eogViewer :: ExternalViewer
eogViewer = ExternalViewer "eog" [] 0


-- | @feh --fullscreen --auto-zoom /tmp/hip/img.tiff@
-- <https://feh.finalrewind.org/ FEH>
fehViewer :: ExternalViewer
fehViewer = ExternalViewer "feh" ["--fullscreen", "--auto-zoom"] 2


-- | @gpicview /tmp/hip/img.tiff@
-- <http://lxde.sourceforge.net/gpicview/ GPicView>
gpicviewViewer :: ExternalViewer
gpicviewViewer = ExternalViewer "gpicview" [] 0


-- | @gimp /tmp/hip/img.tiff@
-- <https://www.gimp.org/ GIMP>
gimpViewer :: ExternalViewer
gimpViewer = ExternalViewer "gimp" [] 0



{- $supported
Encoding and decoding of images is done using
<http://hackage.haskell.org/package/JuicyPixels JuicyPixels> and
<http://hackage.haskell.org/package/netpbm netpbm> packages.
   
List of image formats that are currently supported, and their exact
'ColorSpace's and precision for reading and writing:
  
* 'BMP':

    * __read__: ('Y' 'Word8'), ('RGB'  'Word8'), ('RGBA'  'Word8')
    * __write__: ('Y' 'Word8'), ('RGB'  'Word8'), ('RGBA'  'Word8')

* 'GIF':

    * __read__: ('RGB'  'Word8'), ('RGBA'  'Word8')
    * __write__: ('RGB'  'Word8')
    * Also supports reading and writing animated images, when used as @['GIF']@

* 'HDR':

    * __read__: ('RGB'  'Float')
    * __write__: ('RGB'  'Float')

* 'JPG':

    * __read__: ('Y' 'Word8'), ('YA' 'Word8'), ('RGB'  'Word8'), ('CMYK'  'Word8'),
    ('YCbCr', 'Word8')
    * __write__: ('Y' 'Word8'), ('YA', 'Word8'), ('RGB'  'Word8'), ('CMYK'  'Word8'),
    ('YCbCr', 'Word8')

* 'PNG':

    * __read__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB'  'Word8'), ('RGB'  'Word16'), ('RGBA'  'Word8'), ('RGBA'  'Word16')
    * __write__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB'  'Word8'), ('RGB'  'Word16'), ('RGBA'  'Word8'), ('RGBA'  'Word16')

* 'TGA':

    * __read__: ('Y' 'Word8'), ('RGB'  'Word8'), ('RGBA'  'Word8')
    * __write__: ('Y' 'Word8'), ('RGB'  'Word8'), ('RGBA'  'Word8')

* 'TIF':

    * __read__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB'  'Word8'), ('RGB'  'Word16'), ('RGBA'  'Word8'), ('RGBA'  'Word16'),
    ('CMYK'  'Word8'), ('CMYK'  'Word16')
    * __write__: ('Y' 'Word8'), ('Y' 'Word16'), ('YA' 'Word8'), ('YA' 'Word16'),
    ('RGB'  'Word8'), ('RGB'  'Word16'), ('RGBA'  'Word8'), ('RGBA'  'Word16')
    ('CMYK'  'Word8'), ('CMYK'  'Word16'), ('YCbCr'  'Word8')

* 'PBM':

    * __read__: ('Binary' 'Bit')
    * Also supports sequence of images in one file, when read as @['PBM']@

* 'PGM':

    * __read__: ('Y' 'Word8'), ('Y' 'Word16')
    * Also supports sequence of images in one file, when read as @['PGM']@

* 'PPM':

    * __read__: ('RGB'  'Word8'), ('RGB'  'Word16')
    * Also supports sequence of images in one file, when read as @['PPM']@

-}

