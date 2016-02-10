module Graphics.Image.Repa (
  --readImageGray, readImageGrayA, readImageRGB, readImageRGBA
  ) where


import Graphics.Image.IO
import Graphics.Image.Interface
import Graphics.Image.ColorSpace
import Graphics.Image.Repa.Internal


{-
readImageGray :: FilePath -> IO (Image RD Gray Double)
readImageGray = fmap (either error id) . readImage


readImageGrayA :: FilePath -> IO (Image RD GrayA Double)
readImageGrayA = fmap (either error id) . readImage

readImageRGB :: FilePath -> IO (Image RD RGB Double)
readImageRGB = fmap (either error id) . readImage

readImageRGBA :: FilePath -> IO (Image RD RGBA Double)
readImageRGBA = fmap (either error id) . readImage
-}
