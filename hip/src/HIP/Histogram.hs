module HIP.Histogram (
  getHistograms
  ) where

import Graphics.EasyPlot
import HIP.Conversion (toLists)
import HIP.Interface hiding (map)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

getHistograms :: (Strategy strat img (Inner px), AImage img px,
                  RealFrac (Inner px)) =>
                 strat img (Inner px)
              -> Int
              -> img px
              -> [Graph2D (Inner px) (Inner px)]
getHistograms strat steps img =
  map (Data2D [Style Lines] [Range 0.0 $ fromIntegral steps + 1] .
       V.toList . getHistogram (steps+1) delta . toBoxedVector strat) $ toLists img where
    delta = 1 / fromIntegral steps


--getHistogram' :: Int -> Double -> V.Vector Double -> (V.Vector Double)
getHistogram steps delta v =
  V.zip (V.generate steps fromIntegral) (V.create (makeHistogram steps delta v))


makeHistogram steps delta v = do
    h <- MV.new steps
    MV.set h 0.0
    let count px = do
          let idx = round (px / delta)
          c <- MV.read h idx
          MV.write h idx (c+1)
    V.mapM_ count v
    return h


