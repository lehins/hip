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
import Control.Concurrent (forkIO, ThreadId)
import Data.Char (toLower)
import Data.IORef
import Data.ByteString (readFile)
--import Graphics.EasyPlot hiding (TerminalType(..))
--import qualified Graphics.EasyPlot as Plot (TerminalType(PNG))
import Graphics.Image.Interface
import Graphics.Image.External
--import HIP.Histogram
import qualified Data.ByteString.Lazy as BL (writeFile, hPut)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (takeExtension)
import System.IO (Handle, hFlush, hClose)
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (spawnProcess, waitForProcess, showCommandForUser)


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


writeImage :: Writable (Image arr cs Double) OutputFormat =>
              FilePath
           -> Image arr cs Double
           -> IO ()
writeImage path = BL.writeFile path . encode format [] where
  format = maybe (error ("Could not guess output format. Use 'writeImageExact' "++
                         "or supply a filename with supported format."))
           id (guessFormat path :: Maybe OutputFormat)

  
writeImageExact :: (ManifestArray arr cs e, Writable (Image arr cs e) format) =>
                   format
                -> [SaveOption format]
                -> FilePath
                -> Image arr cs e
                -> IO ()
writeImageExact format opts path = BL.writeFile path . encode format opts
  



{-| Sets the program to be use when displaying an image, where boolean specifies
if current thread should block until the program is closed when calling
'displayImage' function. GPicView ("gpicview", False) is set as a default
program with a nonblocking flag.

    >>> setDisplayProgram ("gpicview", True) -- use gpicview and block current thread.

    >>> setDisplayProgram ("gimp", False) -- use gimp and don't block current thread.
    
    >>> setDisplayProgram ("xv", False)

    >>> setDisplayProgram ("display", False)
-}
setDisplayProgram :: (String, Bool) -> IO ()
setDisplayProgram = writeIORef displayProgram 


displayProgram :: IORef (String, Bool)
displayProgram = unsafePerformIO . newIORef $ ("gpicview", False)
{-# NOINLINE displayProgram #-}


{- | Makes a call to the current display program, which can be changed using
'setDisplayProgram'. An image is written as a @.tiff@ file into an operating
system's temporary directory and passed as an argument to the display
program. If a blocking flag was set to 'False' with 'setDisplayProgram' this
function will return immediatly with 'Just' 'ThreadId', otherwise it will block
current thread until external program is terminated, in which case 'Nothing' is
returned. After display program is closed temporary file is deleted.

  >>> frog <- readImageRGB "images/frog.jpg"
  >>> displayImage frog
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
  BL.hPut h (encode TIF [] img)
  hFlush h
  putStrLn $ showCommandForUser program [path]
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
