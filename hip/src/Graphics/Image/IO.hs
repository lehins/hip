{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module HIP.IO (
  readImage,
  --writeImage, displayImage,
  --displayImageHistograms, displayHistograms, setDisplayProgram, writeHistograms,
  BMP(..), PNG(..), InputFormat,
  Readable(..), ImageFormat
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

import Data.Word
import Graphics.Image.Repa.Internal
import Graphics.Image.ColorSpace

{-
extToInputFormat :: String -> Either String InputFormat
extToInputFormat = ext
  | null ext                     = Left "File extension was not supplied."
  | ext == ".bmp"                = Right BMPin
  | ext == ".gif"                = Right GIFin
  | ext == ".hdr"                = Right HDRin
  | ext `elem` [".jpg", ".jpeg"] = Right JPGin
  | ext == ".png"                = Right PNGin
  | ext == ".tga"                = Right TGAin
  | ext `elem` [".tif", ".tiff"] = Right TIFin
  | ext == ".pbm"                = Right PBMin
  | ext == ".pgm"                = Right PGMin
  | ext == ".ppm"                = Right PPMin
  | otherwise                    = Left $ "Unsupported file extension: "++ext


extToOutputFormat :: String -> Either String OutputFormat
extToOutputFormat ext
  | null ext                     = Left "File extension was not supplied."
  | ext == ".bmp"                = Right BMP
  -- \ | ext == ".gif"                = Right GIF
  | ext == ".hdr"                = Right HDR
  | ext `elem` [".jpg", ".jpeg"] = Right $ JPG 100
  | ext == ".png"                = Right PNG
  -- \ | ext == ".tga"                = Right TGA
  | ext `elem` [".tif", ".tiff"] = Right TIF
  -- \ | ext == ".pbm"                = Right $ PBM RAW
  -- \ | ext == ".pgm"                = Right $ PGM RAW
  -- \ | ext == ".ppm"                = Right $ PPM RAW
  | otherwise                         = Left $ "Unsupported file extension: "++ext
                
-}
guessFormat :: (String -> a) -> FilePath -> a
guessFormat extToFormat = extToFormat . P.map toLower . takeExtension 

--guessOutputFormat :: FilePath -> Either String OutputFormat
--guessOutputFormat = guessFormat extToOutputFormat


--guessInputFormat :: FilePath -> Either String InputFormat
--guessInputFormat = guessFormat extToInputFormat
                   
data InputFormat = InputBMP
                 | InputPNG  deriving (Show, Enum, Eq)


instance ImageFormat InputFormat where

  ext InputBMP = ext BMP
  ext InputPNG = ext PNG

  

instance (Readable img BMP, Readable img PNG) =>
         Readable img InputFormat where
  decode InputBMP = decode BMP
  decode InputPNG = decode PNG


guessInputFormat :: FilePath -> Maybe InputFormat
guessInputFormat path =
  headMaybe . dropWhile (not . isFormat e) . enumFrom . toEnum $ 0
  where e = P.map toLower . takeExtension $ path
        headMaybe ls = if null ls then Nothing else Just $ head ls


readAnyImage :: Readable (Image arr cs Double) InputFormat =>
             FilePath
          -> IO (Either String (Image arr cs Double))
readAnyImage path = do
  imgstr <- readFile path
  let maybeFormat = guessInputFormat path
      formats = enumFrom . toEnum $ 0
      orderedFormats = maybe formats (\f -> f:(filter (/=f) formats)) maybeFormat
      reader (Left err) format = 
        return $ either (Left . ((err++"\n")++)) Right (decode format imgstr)
      reader img         _     = return img
  foldM reader (Left "") orderedFormats


readImage :: Readable img format =>
             format
          -> FilePath
          -> IO (Either String img)
readImage format path = fmap (decode format) (readFile path)


readImageRGB :: FilePath -> IO (Image RP RGB Double)
readImageRGB = fmap (either error id) . readAnyImage

{-


writeImage :: Writeable arr cs e =>
              [SaveOption (]
           -> FilePath
           -> Image arr cs e
           -> IO ()
writeImage !options !path !img =
  BL.writeFile path $ encoder format img where
    !format  = getFormat options
    !encoder = getEncoder options
    getFormat []           = either error id $ guessOutputFormat path
    getFormat (Format f:_) = f
    getFormat (_:opts)     = getFormat opts
    getEncoder []              = defaultEncoder format
    getEncoder (Encoder enc:_) = enc
    getEncoder (_:opts)        = getEncoder opts
    defaultEncoder !f = case f of BMP     -> inRGB8
                                  (JPG _) -> inYCbCr8
                                  PNG     -> inRGB8
                                  TIF     -> inRGB8
                                  HDR     -> inRGBF
                                  -- PBM  -> inY8
                                  -- PGM  -> inY8
                                  -- PPM  -> inRGB8

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
  writeImage strat [Format PNG, Encoder inRGBA8] path img
  ph <- runCommand (program ++ " " ++ path)
  e <- waitForProcess ph
  let printExit ExitSuccess = return ()
      printExit exitCode = print exitCode
  printExit e
-}  

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
