
module Main where

import Graphics.Image as I
import Graphics.Image.ColorSpace
import Graphics.Image.Types


main :: IO ()
main = do
  lena <- readImageRGB "lena.jpg"
  writeImageExact JPG [JPGQuality 100] "lena-result.jpg" $
    I.map toWord8 $ toImageYCbCr $ padImage lena (600, 600)
  return ()



padImage :: Image VU RGB Double -> (Int, Int) -> Image VU RGB Double
padImage img d@(desiredHeight, desiredWidth) = makeImage d getPx
  where getPx (y, x) =
          if x < xstart || x >= xend || y < ystart || y >= yend
          then PixelRGB 1 1 1
          else index img (y - ystart, x - xstart)
        xstart = max 0 (desiredWidth - imageWidth) `quot` 2
        xend   = xstart + imageWidth
        ystart = max 0 (desiredHeight - imageHeight) `quot` 2
        yend   = ystart + imageHeight
        (imageHeight, imageWidth) = dims img
