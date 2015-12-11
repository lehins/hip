{-# LANGUAGE BangPatterns, FlexibleContexts, ViewPatterns #-}
module HIP.IO (
  readImage, writeImage, displayImage,
  displayImageHistograms, displayHistograms, setDisplayProgram, writeHistograms,
  OutputFormat(..), SaveOption(..), Encoder, Saveable(..), Readable
  ) where

import Prelude as P hiding (readFile, writeFile)
import Data.Char (toLower)
import Data.IORef
import Data.ByteString (readFile)
import Graphics.EasyPlot hiding (TerminalType(..))
import qualified Graphics.EasyPlot as Plot (TerminalType(PNG))
import HIP.Interface (AImage, Strategy(compute))
import HIP.Pixel.Base (Pixel(..))
import HIP.External
import HIP.Histogram
import qualified Data.ByteString.Lazy as BL (writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (runCommand, waitForProcess)


extToInputFormat :: String -> Either String InputFormat
extToInputFormat ext
  | null ext                   = Left "File extension was not supplied."
  | ext == ".bmp"              = Right BMPin
  | ext == ".gif"              = Right GIFin
  | ext == ".hdr"              = Right HDRin
  | elem ext [".jpg", ".jpeg"] = Right JPGin
  | ext == ".png"              = Right PNGin
  | ext == ".tga"              = Right TGAin
  | elem ext [".tif", ".tiff"] = Right TIFin
  | ext == ".pbm"              = Right PBMin
  | ext == ".pgm"              = Right PGMin
  | ext == ".ppm"              = Right PPMin
  | otherwise = Left $ "Unsupported file extension: "++ext


extToOutputFormat :: String -> Either String OutputFormat
extToOutputFormat ext
  | null ext                   = Left "File extension was not supplied."
  | ext == ".bmp"              = Right BMP
  -- \ | ext == ".gif"              = Right GIF
  | ext == ".hdr"              = Right HDR
  | elem ext [".jpg", ".jpeg"] = Right $ JPG 100
  | ext == ".png"              = Right PNG
  -- \ | ext == ".tga"              = Right TGA
  | elem ext [".tif", ".tiff"] = Right TIF
  -- \ | ext == ".pbm"              = Right $ PBM RAW
  -- \ | ext == ".pgm"              = Right $ PGM RAW
  -- \ | ext == ".ppm"              = Right $ PPM RAW
  | otherwise = Left $ "Unsupported file extension: "++ext
                

guessFormat :: (String -> a) -> FilePath -> a
guessFormat extToFormat path = extToFormat . P.map toLower . takeExtension $ path

guessOutputFormat :: FilePath -> Either String OutputFormat
guessOutputFormat = guessFormat extToOutputFormat


guessInputFormat :: FilePath -> Either String InputFormat
guessInputFormat = guessFormat extToInputFormat
                   

readImage :: (AImage img px, Readable img px) =>
             FilePath
          -> IO (img px)
readImage path = fmap ((either error id) . (decodeImage maybeFormat)) (readFile path)
  where !maybeFormat = either (const Nothing) Just $ guessInputFormat path  


writeImage :: (Strategy strat img px, AImage img px, Saveable img px) =>
              strat img px
              -> FilePath
              -> img px
              -> [SaveOption img px]
              -> IO ()
writeImage !strat !path !img !options =
  BL.writeFile path $ encoder format (compute strat img) where
    !format = getFormat options
    !encoder = getEncoder options
    getFormat [] = either error id $ guessOutputFormat path
    getFormat !((Format f):_) = f
    getFormat !(_:opts) = getFormat opts
    getEncoder [] = defaultEncoder format
    getEncoder !((Encoder enc):_) = enc
    getEncoder !(_:opts) = getEncoder opts
    defaultEncoder !f = case f of
      BMP     -> inRGB8
      (JPG _) -> inYCbCr8
      PNG     -> inRGB8
      TIF     -> inRGB8
      HDR     -> inRGBF
      -- PBM  -> inY8
      -- PGM  -> inY8
      -- PPM  -> inRGB8

{-| Sets the program to use when making a call to display.  GPicView (gpicview) is
the used as a default program.

    >>> setDisplayProgram "gpicview"

    >>> setDisplayProgram "gimp"
    
    >>> setDisplayProgram "xv"

    >>> setDisplayProgram "display"
 -}
setDisplayProgram :: String -> IO ()
setDisplayProgram = writeIORef displayProgram 


displayProgram :: IORef String
displayProgram = unsafePerformIO $ do
  dp <- newIORef "gpicview"
  return dp


-- | Makes a call to the current display program to be displayed. If the program
-- cannot read from standard in, a file named ".tmp-img" is created and used as
-- an argument to the program.
--
--  >>> frog <- readImageRGB "images/frog.jpg"
--  >>> displayImage frog
--
displayImage :: (Strategy strat img px, AImage img px, Saveable img px) =>
                strat img px
             -> img px
             -> IO ()
displayImage strat img = do
  program <- readIORef displayProgram
  withSystemTempDirectory "hip_" (displayUsing strat img program)


displayUsing :: (Strategy strat img px, AImage img px, Saveable img px) =>
                strat img px -> img px -> String -> FilePath -> IO ()
displayUsing strat img program tmpDir = do
  let path = tmpDir </> "tmp-img.png"
  writeImage strat path img [Format PNG, Encoder inRGBA8]
  ph <- runCommand (program ++ " " ++ path)
  e <- waitForProcess ph
  let printExit ExitSuccess = return ()
      printExit exitCode = print exitCode
  printExit e
  


displayImageHistograms :: (Strategy strat img (Channel px), AImage img px,
                           Enum (Channel px), RealFrac (Channel px)) =>
                          strat img (Channel px)
                       -> Int 
                       -> img px
                       -> IO ()
displayImageHistograms strat steps img = displayHistograms $ getHistograms strat steps img


-- | Displays a list of 'H.Histogram's supplied using an external program that can
-- be changed with 'IO.setDisplayProgram'.
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
writeHistograms path hists = plot (Plot.PNG path) hists
  
  
