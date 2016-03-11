{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Graphics.Image.Histogram (
  Histogram(..), getHistograms, getHistogram,
  writeHistogram
  ) where

import Prelude hiding (map, mapM_)
import qualified Prelude as P (map, mapM_, mapM)
import Control.Monad.Primitive (PrimMonad (..))
import Graphics.Image.Interface
import Graphics.Image.ColorSpace
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Colour.SRGB as C

data Histogram = Histogram { hBins :: V.Vector Int
                           , hName :: String }


getHistograms :: forall arr cs . (SequentialArray arr cs Word8, SequentialArray arr Gray Word8) =>
                 Image arr cs Word8
              -> [Histogram]
getHistograms = P.map setCh . zip (enumFrom (toEnum 0) :: [cs]) . P.map getHistogram . toGrayImages
  where setCh (ch, h) = h { hName = show ch }


getHistogram :: (SequentialArray arr Gray Word8) =>
                Image arr Gray Word8
             -> Histogram
getHistogram img = Histogram { hBins = V.modify countBins $
                                       V.replicate (1 + fromIntegral (maxBound :: Word8)) 0
                             , hName = show Gray } where
  incBin v (PixelGray g) = modify v (+1) $ fromIntegral g
  countBins v = mapM_ (incBin v) img
  
             

modify :: (PrimMonad m, V.Unbox a) => MV.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify v f ix = do
  e <- MV.read v ix
  MV.write v ix $ f e


writeHistogram fileName hists = toFile def fileName $ do
  layout_title .= "Histogram"
  setColors $ fmap opaque ([C.sRGB 1 0 0, C.sRGB 0 1 0, C.sRGB 0 0 1])
  --layoutlr_left_axis . laxis_override .= axisGridHide
  P.mapM_ plotHist hists where
    plotHist h = do
      plot (line (hName h) [V.toList $ V.imap (,) (hBins h)])
