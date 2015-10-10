{-# LANGUAGE BangPatterns, FlexibleContexts, ViewPatterns #-}
module Graphics.Image.IO (
  readImage, writeImage, display, setDisplayProgram
  ) where

import Prelude as P hiding (readFile, writeFile)
import Codec.Picture.Types (DynamicImage)
import Data.Char (toUpper)
import Data.IORef
import Data.ByteString (readFile)
import Graphics.Image.Conversion
import Graphics.Image.Interface (Image, Convertable, Strategy(compute))
import Graphics.Netpbm (PPM)
import qualified Data.ByteString.Lazy as BL (writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (runCommand, waitForProcess)


readImage :: (Image img px,
              Convertable DynamicImage (img px),
              Convertable PPM          (img px)) =>
             FilePath -> IO (img px)
readImage path = fmap ((either error id) . decodeImage) (readFile path)


extToFormat :: [Char] -> Format
extToFormat ((P.map toUpper) -> ext)
  | ext == "BMP"             = BMP
  | elem ext ["JPG", "JPEG"] = JPG 100
  | ext == "PNG"             = PNG
  | elem ext ["TIF", "TIFF"] = TIFF
  | ext == "HDR"             = HDR
  --  ext == "PBM"             = PBM
  --  ext == "PGM"             = PGM
  --  ext == "PPM"             = PPM
  | null ext = error "File extension was not supplied"
  | otherwise = error $ "Unsupported file extension: "++ext


writeImage :: (Strategy strat img px, Image img px, Saveable img px) =>
              strat img px
              -> FilePath
              -> img px
              -> [SaveOption img px]
              -> IO ()
writeImage !strat !path !img !options =
  BL.writeFile path $ encoder format (compute strat img) where
    !format = getFormat options
    !encoder = getEncoder options
    !ext = reverse . fst . (span ('.'/=)) . reverse $ path
    getFormat [] = extToFormat ext
    getFormat !((Format f):_) = f
    getFormat !(_:opts) = getFormat opts
    getEncoder [] = defaultEncoder format
    getEncoder !((Encoder enc):_) = enc
    getEncoder !(_:opts) = getEncoder opts
    defaultEncoder !f = case f of
      BMP    -> inRGB8
      (JPG _)-> inYCbCr8
      PNG    -> inRGB8
      TIFF   -> inRGB8
      HDR    -> inRGBF
      -- PBM  -> inY8
      -- PGM  -> inY8
      -- PPM  -> inRGB8

{-| Sets the program to use when making a call to display. By default,
    ImageMagick (display) is the default program to use and it is read
    using stdin.

    >>> setDisplayProgram "gpicview"

    >>> setDisplayProgram "gimp"
    
    >>> setDisplayProgram "xv"

    >>> setDisplayProgram "display"
 -}
setDisplayProgram :: String -> IO ()
setDisplayProgram = writeIORef displayProgram 


displayProgram :: IORef String
displayProgram = unsafePerformIO $ do
  dp <- newIORef "display"
  return dp


{-| Makes a call to the current display program to be displayed. If the
    program cannot read from standard in, a file named ".tmp-img" is created
    and used as an argument to the program.

    >>>frog <- readImage "images/frog.pgm"
    >>>display frog

 -}
display :: (Strategy strat img px, Image img px, Saveable img px) =>
           strat img px
        -> img px
        -> IO ()
display strat img = do
  program <- readIORef displayProgram
  withSystemTempDirectory "hip_" (displayUsing strat img program)


displayUsing :: (Strategy strat img px, Image img px, Saveable img px) =>
                strat img px -> img px -> String -> FilePath -> IO ()
displayUsing strat img program tmpDir = do
  let path = tmpDir </> "tmp-img.png"
  writeImage strat path img [Format PNG]
  ph <- runCommand (program ++ " " ++ path)
  e <- waitForProcess ph
  let printExit ExitSuccess = return ()
      printExit exitCode = print exitCode
  printExit e
  
