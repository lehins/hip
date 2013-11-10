module Main where
import System.Environment
import Data.Image
import Data.Image.IO
import Data.Image.Convolution

main = do
  [f] <- getArgs
  (Right im) <- readColorImage f
  writeImage ("mod-"++f) (convolve im) JPG inYCbCr8
