{-# LANGUAGE BangPatterns, FlexibleContexts, ViewPatterns #-}
module Graphics.Image.Interface.IO (
  readImage, writeImage, display, setDisplayProgram
  ) where

import Prelude as P hiding (readFile, writeFile)
import Data.Char (toLower)
import Data.IORef
import Data.ByteString (readFile)
import Graphics.Image.Interface (Image, Strategy(compute))
import Graphics.Image.Interface.Conversion
import qualified Data.ByteString.Lazy as BL (writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (runCommand, waitForProcess)


extToFormat :: String -> Either String Format
extToFormat ext
  | null ext                   = Left "File extension was not supplied."
  | ext == ".bmp"              = Right BMP
  | ext == ".gif"              = Right GIF
  | ext == ".hdr"              = Right HDR
  | elem ext [".jpg", ".jpeg"] = Right $ JPG 100
  | ext == ".png"              = Right PNG
  | ext == ".tga"              = Right TGA
  | elem ext [".tif", ".tiff"] = Right TIF
  | ext == ".pbm"              = Right $ PBM RAW
  | ext == ".pgm"              = Right $ PGM RAW
  | ext == ".ppm"              = Right $ PPM RAW
  | otherwise = Left $ "Unsupported file extension: "++ext
                

isNetpbm (PBM _) = True
isNetpbm (PGM _) = True
isNetpbm (PPM _) = True
isNetpbm _       = False


guessFormat :: FilePath -> Either String Format
guessFormat path = extToFormat . P.map toLower . takeExtension $ path
  

readImage :: (Image img px, Readable img px) =>
             FilePath -> IO (img px)
readImage path = 
  fmap ((either error id) . (decodeImage maybeFormat)) (readFile path) where 
    !maybeFormat = either (const Nothing) Just $ guessFormat path  



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
    getFormat [] = either error id $ guessFormat path
    getFormat !((Format f):_) = f
    getFormat !(_:opts) = getFormat opts
    getEncoder [] = defaultEncoder format
    getEncoder !((Encoder enc):_) = enc
    getEncoder !(_:opts) = getEncoder opts
    defaultEncoder !f = case f of
      BMP    -> inRGB8
      (JPG _)-> inYCbCr8
      PNG    -> inRGB8
      TIF    -> inRGB8
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
  
