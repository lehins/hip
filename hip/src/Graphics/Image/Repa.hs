module Graphics.Image.Repa (
  module Graphics.Image.Repa.Internal,
  readImageY, readImageYA, readImageRGB, readImageRGBA
  ) where


import Graphics.Image.IO
import Graphics.Image.Interface
import Graphics.Image.ColorSpace
import Graphics.Image.Repa.Internal



readImageY :: FilePath -> IO (Image RD Y Double)
readImageY = fmap (either error id) . readImage


readImageYA :: FilePath -> IO (Image RD YA Double)
readImageYA = fmap (either error id) . readImage


readImageRGB :: FilePath -> IO (Image RD RGB Double)
readImageRGB = fmap (either error id) . readImage


readImageRGBA :: FilePath -> IO (Image RD RGBA Double)
readImageRGBA = fmap (either error id) . readImage
