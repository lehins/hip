{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Graphics.Image.IO (
  readImage, readImageExact, writeImage, writeImageExact,
  displayImage, setDisplayProgram, 
  --displayImageHistograms, displayHistograms, writeHistograms,
  InputFormat, OutputFormat,
  BMP(..), GIF(..), HDR(..), JPG(..), PNG(..), TGA(..), TIF(..),
  Readable(..), Writable(..), ImageFormat
  ) where

import Prelude as P hiding (readFile, writeFile)
import Control.Monad (foldM)
import Data.Char (toLower)
import Data.IORef
import Data.ByteString (readFile)
--import Graphics.EasyPlot hiding (TerminalType(..))
--import qualified Graphics.EasyPlot as Plot (TerminalType(PNG))
import Graphics.Image.Interface
import Graphics.Image.External
--import HIP.Histogram
import qualified Data.ByteString.Lazy as BL (writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (runCommand, waitForProcess)


guessFormat :: (ImageFormat f, Enum f) => FilePath -> Maybe f
guessFormat path =
  headMaybe . dropWhile (not . isFormat e) . enumFrom . toEnum $ 0
  where e = P.map toLower . takeExtension $ path
        headMaybe ls = if null ls then Nothing else Just $ head ls

-- | This function will try to guess an image format from file's extension and
-- attempt to read it as such first, while falling back on the rest of the
-- supported formats. Image will be read into a type signature specified
-- 'ColorSpace' with 'Double' precision, while doing all necessary
-- conversions. Whenever image cannot be decoded, 'Left' containing an error
-- will be returned, and 'Right' containing an image otherwise.
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


readImageExact :: Readable img format =>
                  format
               -> FilePath
               -> IO (Either String img)
readImageExact format path = fmap (decode format) (readFile path)


writeImage :: Writable (Image arr cs e) OutputFormat =>
              FilePath
           -> Image arr cs e
           -> IO ()
writeImage path = BL.writeFile path . encode format [] where
  format = maybe (error ("Could not guess output format. Use 'writeImageExact' "++
                         "or supply a filename with supported format."))
           id (guessFormat path :: Maybe OutputFormat)

  
writeImageExact :: Writable img format =>
                   FilePath
                -> format
                -> [SaveOption format]
                -> img
                -> IO ()
writeImageExact path format opts = BL.writeFile path . encode format opts
  



{-| Sets the program to use when making a call to display.  GPicView (gpicview) is
set as a default program.

    >>> setDisplayProgram "gpicview"

    >>> setDisplayProgram "gimp"
    
    >>> setDisplayProgram "xv"

    >>> setDisplayProgram "display"
 -}
setDisplayProgram :: String -> IO ()
setDisplayProgram = writeIORef displayProgram 


displayProgram :: IORef String
displayProgram = unsafePerformIO . newIORef $ "gpicview"
{-# NOINLINE displayProgram #-}


-- | Makes a call to the current display program to be displayed. If the program
-- cannot read from standard in, a file named ".tmp-img" is created and used as
-- an argument to the program.
--
--  >>> frog <- readImageRGB "images/frog.jpg"
--  >>> displayImage frog
--
displayImage :: Writable (Image arr cs e) OutputFormat =>
                Image arr cs e
             -> IO ()
displayImage img = do
  program <- readIORef displayProgram
  withSystemTempDirectory "hip_" (displayUsing img program)


displayUsing :: Writable (Image arr cs e) OutputFormat =>
                Image arr cs e -> String -> FilePath -> IO ()
displayUsing img program tmpDir = do
  let path = tmpDir </> "tmp-img.png"
  writeImage path img
  ph <- runCommand (program ++ " " ++ path)
  e <- waitForProcess ph
  let printExit ExitSuccess = return ()
      printExit exitCode = print exitCode
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
