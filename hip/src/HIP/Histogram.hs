module HIP.Histogram (
  Histogram, getHistograms, getHistogramsUsing, getHistogramVector
  ) where

import Graphics.EasyPlot
import HIP.Conversion (toLists)
import HIP.Interface hiding (map)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import Control.Monad.Primitive


-- | Histograms are plotted using <http://hackage.haskell.org/package/easyplot EasyPlot>.
type Histogram a = Graph2D a a


getHistograms :: (Strategy strat img (Inner px), AImage img px, RealFrac (Inner px)) =>
                 strat img (Inner px)
              -> Int
              -> img px
              -> [Histogram (Inner px)]
getHistograms strat bins img = getHistogramsUsing bins img maker where
  delta = 1 / fromIntegral bins
  maker = V.toList . getHistogramVector (bins+1) delta . toBoxedVector strat


getHistogramsUsing :: (AImage img px, AImage img (Inner px), Fractional (Inner px)) =>
                      Int -- ^ Number of bins histograms should have.
                   -> img px -- ^ Image that will have it's pixels counted for
                             -- these histograms.
                   -> (img (Inner px) -> [(Inner px, Inner px)])
                   -> [Histogram (Inner px)]
getHistogramsUsing bins img maker = map makeHistogram $ toLists img where
  makeHistogram = Data2D [Style Lines] [Range 0.0 $ fromIntegral bins + 1] . maker


getHistogramVector :: (GV.Vector v1 a1, GV.Vector v (a, b), GV.Vector v b,
                 GV.Vector v a, RealFrac a1, Num a, Fractional b) =>
                Int -> a1 -> v1 a1 -> v (a, b)
getHistogramVector bins delta v =
  GV.zip (GV.generate bins fromIntegral) (GV.create (count bins delta v))


count :: (GV.Vector v1 a1, GMV.MVector v a,
                  PrimMonad m, RealFrac a1,
                  Fractional a) =>
                 Int
              -> a1
              -> v1 a1
              -> m (v (PrimState m) a)
count bins delta v = do
    h <- GMV.new bins
    GMV.set h 0.0
    let counter px = do
          let idx = round (px / delta)
          c <- GMV.read h idx
          GMV.write h idx (c+1)
    GV.mapM_ counter v
    return h


