{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Chart
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Chart where

import Data.Semigroup
import Data.List.NonEmpty as NE
import qualified Data.Massiv.Array as A
import Control.Monad as M
import qualified Data.Colour.SRGB as Colour
import Graphics.Color.Model as Color
import Graphics.Image.Processing.Histogram
--import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

plotHistograms :: Histograms -> EC (Layout Int Int) ()
plotHistograms (Histograms hs) = do
  layout_title .= "Image Histogram"
  setColors $ NE.toList $ fmap (toColourRGBA . histogramColor) hs
  let tick = 20
      axis = set la_nTicks tick . set la_nLabels 14
      len = getMax $ sconcat $ fmap (Max . A.unSz . A.size . histogramBins) hs
      upper =
        case len `quotRem` tick of
          (_, 0) -> len
          (q, _) -> q * tick + tick
  layout_x_axis . laxis_generate .= scaledIntAxis (axis defaultIntAxis) (0, upper)
  M.forM_ hs $ \h -> plot $ line (histogramName h) [A.stoList . A.simap (,) $ histogramBins h]


toColourRGBA :: Color (Alpha RGB) Word8 -> AlphaColour Double
toColourRGBA (fmap toDouble -> Alpha (Color.ColorRGB r g b) a) = withOpacity (Colour.sRGB r g b) a
