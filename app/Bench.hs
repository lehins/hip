module Main where

import           Criterion.Main
import Graphics.Image as I
import           Prelude                     as P


main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
      i = makeImage (256 :. 256) (\(i :. _) -> PixelY (fromIntegral i)) :: I.Image Y Word8
  defaultMain
    [ bgroup
        "upsample"
        [ upsample i]]
