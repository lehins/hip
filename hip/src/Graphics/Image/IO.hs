{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Graphics.Image.IO (
  -- * Reading
  readImage, readImageExact,
  -- * Writing
  writeImage, writeImageExact,
  -- * Displaying
  displayImage, setDisplayProgram, 
  --displayImageHistograms, displayHistograms, writeHistograms,
  module Graphics.Image.External
  ) where

import Prelude as P hiding (readFile, writeFile)
import Control.Monad (foldM)
import Control.Concurrent (forkIO, ThreadId)
import Data.Char (toLower)
import Data.IORef
import Data.ByteString (readFile)
--import Graphics.EasyPlot hiding (TerminalType(..))
--import qualified Graphics.EasyPlot as Plot (TerminalType(PNG))
import Graphics.Image.Interface
import Graphics.Image.External.Base
import Graphics.Image.External
--import HIP.Histogram
import qualified Data.ByteString.Lazy as BL (writeFile, hPut)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (takeExtension)
import System.IO (Handle, hFlush, hClose, hPutStrLn, stdout)
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (spawnProcess, waitForProcess, showCommandForUser)


guessFormat :: (ImageFormat f, Enum f) => FilePath -> Maybe f
guessFormat path =
  headMaybe . dropWhile (not . isFormat e) . enumFrom . toEnum $ 0
  where e = P.map toLower . takeExtension $ path
        headMaybe ls = if null ls then Nothing else Just $ head ls

-- | This function will try to guess an image format from file's extension,
-- then it will attempt to read it as such. It will fall back onto the rest of
-- the supported formats and will try to read them regarless of file's
-- extension. Whenever image cannot be decoded, 'Left' containing all errors for
-- each attmpted format will be returned, and 'Right' containing an image
-- otherwise. Image will be read into a type signature specified 'ColorSpace'
-- ('Graphics.Image.ColorSpace.Y', 'Graphics.Image.ColorSpace.YA',
-- 'Graphics.Image.ColorSpace.RGB' and 'Graphics.Image.ColorSpace.RGBA' only)
-- with 'Double' precision, while doing all necessary conversions.
readImage :: Readable (Image arr cs Double) InputFormat =>
             FilePath
          -> IO (Either String (Image arr cs Double))
readImage path = do
  imgstr <- readFile path
  let maybeFormat = (guessFormat path :: Maybe InputFormat)
      formats = enumFrom . toEnum $ 0
      orderedFormats = maybe formats (\f -> f:(filter (/=f) formats)) maybeFormat
      reader (Left err) format = 
        return $ either (Left . ((err++"\n")++)) Right (decode format imgstr)
      reader img         _     = return img
  foldM reader (Left "") orderedFormats

-- | This function allows for reading any supported image in the exact colorspace
-- and precision it is currently encoded in. For instance, frog image can be
-- read into it's 'Graphics.Image.ColorSpace.YCbCr' colorspace with
-- 'Graphics.Image.ColorSpace.Word8' precision and into any supported array
-- representation.
--
-- >>> readImageExact JPG "images/frog.jpg" :: IO (Either String (Image RP YCbCr Word8))
-- Right <Image RepaDelayed YCbCr: 200x320>
--
-- The drawback here is that colorspace and precision has to match exactly,
-- otherwise it will return an error:
--
-- >>> readImageExact JPG "images/frog.jpg" :: IO (Either String (Image RD RGB Word8))
-- Left "JuicyPixel decoding error: Input image is in YCbCr8 (Pixel YCbCr Word8), cannot convert it to RGB8 (Pixel RGB Word8) colorspace."
--
-- Trying to read an image in a particular color space that is not supported by
-- the format, will result in a compile error. Refer to 'Readable' class for all
-- images that can be decoded.
readImageExact :: Readable img format =>
                  format -- ^ A format that an image should be read
                         -- as. Currently supported formats are: 'BMP', 'GIF',
                         -- ['GIF'] (animated GIF), 'HDR', 'JPG', 'PNG', 'TGA'
                         -- and 'TIF'.
               -> FilePath -- ^ Location of an image.
               -> IO (Either String img)
readImageExact format path = fmap (decode format) (readFile path)


-- | Just like 'readImage', this function will guess an output format from the file
-- extension and write to file any image that is in a
-- 'Graphics.Image.ColorSpace.Y', 'Graphics.Image.ColorSpace.YA',
-- 'Graphics.Image.ColorSpace.RGB' or 'Graphics.Image.ColorSpace.RGBA'
-- 'ColorSpace' with 'Double' precision. While doing necessary conversions the
-- choice will be given to the most suited color space supported by the format,
-- for instance, in case of a 'PNG' format, an ('Image' @arr@
-- 'Graphics.Image.ColorSpace.RGBA' 'Double') would be written as @RGBA16@,
-- hence preserving transparency and using highest supported precision
-- 'Graphics.Image.ColorSpace.Word16'. At the same writing that image in
-- 'GIF' format would save it in @RGB8@, since encoding transparent images in
-- 'GIF' is currently not supported.
writeImage :: (ManifestArray arr cs Double, Writable (Image arr cs Double) OutputFormat) =>
              FilePath            -- ^ Location where an image should be written.
           -> Image arr cs Double -- ^ An image to write. 
           -> IO ()
writeImage path = BL.writeFile path . encode format [] where
  format = maybe (error ("Could not guess output format. Use 'writeImageExact' "++
                         "or supply a filename with supported format."))
           id (guessFormat path :: Maybe OutputFormat)

  
writeImageExact :: Writable img format =>
                   format -- ^ A format that an image should be saved
                          -- in. Currently supported formats are: 'BMP', 'GIF',
                          -- ['GIF'] (animated GIF), 'HDR', 'JPG', 'PNG', 'TGA'
                          -- and 'TIF'.
                -> [SaveOption format] -- ^ A list of format specific options.
                -> FilePath -- ^ Location where an image should be written.
                -> img -- ^ An image to write. Can be a list of images in case
                       -- of formats supporting animation.
                -> IO ()
writeImageExact format opts path = BL.writeFile path . encode format opts
  

-- | Sets the program to be use when displaying an image, where boolean
-- specifies if current thread should block until the program is closed when
-- calling 'displayImage' function. GPicView ("gpicview", False) is set as a
-- default program with a nonblocking flag. Here are some examples:
--
-- >>> setDisplayProgram ("gpicview", True) -- use gpicview and block current thread.
-- >>> setDisplayProgram ("gimp", False) -- use gimp and don't block current thread.
-- >>> setDisplayProgram ("xv", False)
-- >>> setDisplayProgram ("display", False)
-- >>> setDisplayProgram ("rm", True) -- :-)
--
setDisplayProgram :: (String, Bool) -> IO ()
setDisplayProgram = writeIORef displayProgram 


displayProgram :: IORef (String, Bool)
displayProgram = unsafePerformIO . newIORef $ ("gpicview", False)
{-# NOINLINE displayProgram #-}


{- | Makes a call to the current display program, which can be changed using
'setDisplayProgram'. An image is written as a @.tiff@ file into an operating
system's temporary directory and passed as an argument to the display program
(an actual command executed will be printed to @stdout@). If a blocking flag was
set to 'False' using 'setDisplayProgram', then function will return immediately
with ('Just' 'ThreadId'), otherwise it will block current thread until external
program is terminated, in which case 'Nothing' is returned. After display
program is closed temporary file is deleted.

  >>> frog <- readImageRGB "images/frog.jpg"
  >>> tid <- displayImage $ computeS frog
  gpicview /tmp/tmp-img13804.tiff
  >>> tid
  Just ThreadId 525
  >>> setDisplayProgram ("gpicview", True)
  >>> displayImage $ computeS frog
  gpicview /tmp/tmp-img13804.tiff
  Nothing
-}
displayImage :: (ManifestArray arr cs e, Writable (Image arr cs e) TIF) =>
                Image arr cs e -- ^ Image to be displayed
             -> IO (Maybe ThreadId)
displayImage img = do
  (program, block) <- readIORef displayProgram
  if block
    then withSystemTempFile "tmp-img.tiff" (displayUsing img program) >> return Nothing
    else forkIO (withSystemTempFile "tmp-img.tiff" (displayUsing img program)) >>= (return . Just)


displayUsing :: (ManifestArray arr cs e, Writable (Image arr cs e) TIF) =>
                Image arr cs e -> String -> FilePath -> Handle -> IO ()
displayUsing img program path h = do
  hPutStrLn stdout $ showCommandForUser program [path]
  hFlush stdout
  BL.hPut h (encode TIF [] img)
  hFlush h
  ph <- spawnProcess program [path]
  e <- waitForProcess ph
  let printExit ExitSuccess = return ()
      printExit exitCode = print exitCode
  hClose h
  printExit e


{-
displayImageHistograms :: (Strategy strat img (Channel px), AImage img px,
                           Enum (Channel px), RealFrac (Channel px)) =>
                          strat img (Channel px)
                       -> Int 
                       -> img px
                       -> IO ()
displayImageHistograms strat steps img = displayHistograms $ getHistograms strat steps img


-- | Displays a list of 'Histogram's supplied using an external program that can
-- be changed with 'setDisplayProgram'.
--
-- >>> centaurus <- readImageRGB "images/centaurus.jpg"
-- >>> cluster <- readImageRGB "images/cluster.jpg" 
-- >>> displayHistograms ((getHistograms 255 centaurus)++(getHistograms 255 cluster))
--
-- <<images/centaurus_cluster_histogram.png>>
--
displayHistograms :: (Show a, Num a, Fractional a, Enum a) => [Histogram a] -> IO ()
displayHistograms hists = do
  program <- readIORef displayProgram
  withSystemTempDirectory "hip_" (displayHistogramsUsing hists program)


displayHistogramsUsing :: (Show a, Num a, Fractional a, Enum a) =>
                          [Histogram a] -> String -> FilePath -> IO ()
displayHistogramsUsing hists program tmpDir = do
  let path = tmpDir </> "tmp-hist.png"
  wrote <- writeHistograms path hists
  if wrote
    then do ph <- runCommand (program ++ " " ++ path)
            e <- waitForProcess ph
            let printExit ExitSuccess = return ()
                printExit exitCode = print exitCode
            printExit e
    else print "Was unsuccessfull in using gnuplot."


-- | Writes histograms into a PNG file image.
--
-- >>> centaurus <- readImageRGB "images/centaurus.jpg"
-- >>> cluster <- readImageRGB "images/cluster.jpg"
-- >>> let histograms = ((getHistograms 255 centaurus)++(getHistograms 255 cluster)) 
-- >>> writeHistograms "images/centaurus_cluster_histogram.png" histograms
-- True
--
writeHistograms :: (Show a, Num a, Fractional a, Enum a) =>
                   FilePath -- ^ PNG image file name.
                -> [Histogram a] -- ^ List of histograms to be plotted.
                -> IO Bool -- ^ Returns 'True' in case of success.
writeHistograms path = plot (Plot.PNG path)
  
  
-}
