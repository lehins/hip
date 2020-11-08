{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Graphics.Image.Chart
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Chart where

import Prelude as P
import Data.List.NonEmpty as NE
import Data.Massiv.Array as A
import Control.Monad as M
import qualified Data.Colour.SRGB as Colour
import Graphics.Image as I
--import Graphics.Image.IO
import Graphics.Image.Processing.Histogram
--import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

-- import qualified Graphics.Color.Space as CS
-- import Graphics.Color.Space (Linearity(..), luminance)
-- import Graphics.Color.Space.RGB.Luma
-- import qualified Graphics.Color.Space.RGB.Alternative.YCbCr as YCbCr
-- import Graphics.Color.Space.RGB.SRGB as SRGB
-- import qualified Graphics.Color.Space.RGB.ITU.Rec470 as Rec

-- class ChannelColour cs where

--   -- | Get a pure colour representation of a channel.
--   csColour :: cs -> C.AlphaColour Double


-- -- | A single channel histogram of an image.
-- data Histogram = Histogram { hBins   :: V.Vector Int
--                              -- ^ Vector containing pixel counts. Index of a
--                              -- vector serves as an original pixel value.
--                            , hName   :: String
--                              -- ^ Name of the channel that will be displayed in
--                              -- the legend.
--                            , hColour :: C.AlphaColour Double
--                              -- ^ Color of a plotted line.
--                            }
-- -- | For now it is just a type synonym, but in the future it might become a custom
-- -- data type with fields like title, width, heigth, etc.
-- type Histograms = [Histogram]


-- -- | Create a histogram per channel with 256 bins each.
-- getHistograms :: forall arr cs e . (ChannelColour cs, MArray arr X e, Array arr X e,
--                                     MArray arr cs e, Array arr cs e) =>
--                  Image arr cs e
--               -> Histograms
-- getHistograms = P.zipWith setCh (enumFrom (toEnum 0) :: [cs]) . P.map getHistogram . toImagesX
--   where setCh !cs !h = h { hName = show cs
--                          , hColour = csColour cs }
--         {-# INLINE setCh #-}
-- {-# INLINE getHistograms #-}

-- | Write histograms into a PNG image file.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> writeHistograms "images/frog_histogram.svg" $ getHistograms frog
--
-- <<images/frog_histogram.svg>>
--
-- writeHistograms :: FilePath -> Histograms -> IO ()
-- writeHistograms fileName hists =
--   toFile def fileName $ do
--plotHistograms :: Histograms -> IO ()
plotHistograms :: Histograms -> EC (Layout Ix1 Int) ()
plotHistograms (Histograms histograms) = do
  layout_title .= "Image Histogram"
  setColors $ NE.toList $ fmap (toColourRGBA . histogramColor) histograms
  let tick = 20
      axis = set la_nTicks tick . set la_nLabels 14
      len = P.maximum $ fmap (unSz . size . histogramBins) histograms
      upper =
        case len `quotRem` tick of
          (_, 0) -> len
          (q, _) -> q * tick + tick
  layout_x_axis . laxis_generate .= scaledIntAxis (axis defaultIntAxis) (0, upper)
  M.forM_ histograms $ \h -> plot $ line (histogramName h) [stoList . simap (,) $ histogramBins h]


toColourRGBA :: (Ord e, Floating e) => Color (Alpha (SRGB 'NonLinear)) e -> AlphaColour e
toColourRGBA (Alpha (ColorSRGB r g b) a) = withOpacity (Colour.sRGB r g b) a


-- plotDiff :: EC (Layout Double Double) ()
-- plotDiff = do
--   setColors $ fmap toColourRGBA [ColorRGBA 1 0 0 1, ColorRGBA 0 1 0 1, ColorRGBA 0 0 1 1]
--   let cs = [ColorSRGB 0 0 r | r <- [0 .. 255]] :: [Color (SRGB 'NonLinear) Word8]
--       ys' = ((\(Y' y') -> y') . rgbLuma <$> cs) :: [Double]
--       ys = ((\(CS.Y y) -> y) . luminance <$> cs) :: [Double]
--       ycbcr = [YCbCr.ColorYCbCr y' 0.5 0.5 :: Color (YCbCr.YCbCr (SRGB 'NonLinear)) Double | y' <- ys']
--       nlys =(((\(CS.Y y) -> y) . luminance . YCbCr.fromColorYCbCr @_ @_ @_ @Double) <$> ycbcr) :: [Double]
--       -- nlys =
--       --   fmap
--       --     ((\(CS.Y y) -> y) . luminance)
--       --     [ColorSRGB y' y' y' :: Color (SRGB 'NonLinear) Double | y' <- ys']
--       step = 1 / 255 :: Double
--       axis = set la_nTicks 20 . set la_nLabels 14
--   layout_x_axis . laxis_generate .= scaledAxis (axis def) (0, 1)
--   plot $ line "Luma" [P.zip [0,step ..] ys']
--   plot $ line "Luminance" [P.zip [0,step ..] ys]
--   plot $ line "Non-Linear luminance" [P.zip [0,step ..] nlys]

-- -- | Display image histograms using an external program. Works in a similar way as
-- -- `Graphics.Image.IO.displayImage`.
-- --
-- -- >>> frog <- readImageRGB VU "images/frog.jpg"
-- -- >>> displayHistograms $ getHistograms frog
-- --
-- displayHistograms :: Histograms -> IO ()
-- displayHistograms = displayHistogramsUsing defaultViewer False


-- instance ChannelColour X where
--   csColour _ = C.opaque C.darkgray

-- instance ChannelColour Y where
--   csColour _ = C.opaque C.darkgray

-- instance ChannelColour YA where
--   csColour LumaYA  = csColour LumaY
--   csColour AlphaYA = C.opaque C.gray


-- instance ChannelColour RGB where
--   csColour RedRGB   = C.opaque C.red
--   csColour GreenRGB = C.opaque C.green
--   csColour BlueRGB  = C.opaque C.blue

-- instance ChannelColour RGBA where
--   csColour RedRGBA   = C.opaque C.red
--   csColour GreenRGBA = C.opaque C.green
--   csColour BlueRGBA  = C.opaque C.blue
--   csColour AlphaRGBA = C.opaque C.gray


-- instance ChannelColour HSI where
--   csColour HueHSI = C.opaque C.purple
--   csColour SatHSI = C.opaque C.orange
--   csColour IntHSI = C.opaque C.darkblue

-- instance ChannelColour HSIA where
--   csColour HueHSIA   = C.opaque C.purple
--   csColour SatHSIA   = C.opaque C.orange
--   csColour IntHSIA   = C.opaque C.darkblue
--   csColour AlphaHSIA = C.opaque C.gray


-- instance ChannelColour CMYK where
--   csColour CyanCMYK = C.opaque C.cyan
--   csColour MagCMYK  = C.opaque C.magenta
--   csColour YelCMYK  = C.opaque C.yellow
--   csColour KeyCMYK  = C.opaque C.black

-- instance ChannelColour CMYKA where
--   csColour CyanCMYKA  = csColour CyanCMYK
--   csColour MagCMYKA   = csColour MagCMYK
--   csColour YelCMYKA   = csColour YelCMYK
--   csColour KeyCMYKA   = csColour KeyCMYK
--   csColour AlphaCMYKA = C.opaque C.grey


-- instance ChannelColour YCbCr where
--   csColour LumaYCbCr  = C.opaque C.darkgray
--   csColour CBlueYCbCr = C.opaque C.darkblue
--   csColour CRedYCbCr  = C.opaque C.darkred


-- instance ChannelColour YCbCrA where
--   csColour LumaYCbCrA  = csColour LumaYCbCr
--   csColour CBlueYCbCrA = csColour CBlueYCbCr
--   csColour CRedYCbCrA  = csColour CRedYCbCr
--   csColour AlphaYCbCrA = C.opaque C.gray
