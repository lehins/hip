{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}
-- |
-- Module      : Graphics.Image.IO.Histogram
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Histogram (
  Histogram(..), getHistograms, getHistogram,
  writeHistograms
  ) where

import Prelude hiding (map, mapM_, zipWith)
import qualified Prelude as P (map, mapM_, zipWith)
import Control.Monad.Primitive (PrimMonad (..))
import Graphics.Image.Interface
import Graphics.Image.ColorSpace
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Colour as C


data Histogram = Histogram { hBins :: V.Vector Int
                           , hName :: String
                           , hColour :: C.AlphaColour Double}


-- | Create a histogram per channel with 256 bins each.
getHistograms :: forall arr cs e . (SequentialArray arr Gray e,
                                    SequentialArray arr cs e, Elevator e) =>
                 Image arr cs e
              -> [Histogram]
getHistograms = P.zipWith setCh (enumFrom (toEnum 0) :: [cs]) . P.map getHistogram . toGrayImages
  where setCh cs h = h { hName = show cs
                       , hColour = csColour cs }

-- | Generate a histogram with 256 bins.
getHistogram :: (SequentialArray arr Gray e, Elevator e) =>
                Image arr Gray e
             -> Histogram
getHistogram img = Histogram { hBins = V.modify countBins $
                                       V.replicate
                                       (1 + fromIntegral (maxBound :: Word8)) (0 :: Int)
                             , hName = show Gray
                             , hColour = csColour Gray } where
  incBin v (toWord8 -> (PixelGray g)) = modify v (+1) $ fromIntegral g
  countBins v = mapM_ (incBin v) img
  

-- | Write histograms into a PNG image file.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeHistograms "images/frog_histogram.png" $ getHistograms frog
--
-- <<images/frog_histogram.png>>
--
writeHistograms :: FilePath -> [Histogram] -> IO ()
writeHistograms fileName hists = toFile def fileName $ do
  layout_title .= "Histogram"
  setColors $ P.map hColour hists
  let axis = set la_nTicks 20 . set la_nLabels 14
  layout_x_axis . laxis_generate .= scaledIntAxis (axis defaultIntAxis) (0, 260)
  P.mapM_ plotHist hists where
    plotHist h = plot (line (hName h) [V.toList $ V.imap (,) $ hBins h])
             

modify :: (PrimMonad m, V.Unbox a) => MV.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify v f idx = do
  e <- MV.read v idx
  MV.write v idx $ f e
