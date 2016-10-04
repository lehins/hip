{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
  displayHistograms, writeHistograms
  ) where

import Prelude as P 
import Control.Concurrent (forkIO)
import Control.Monad.Primitive (PrimMonad (..))
import Graphics.Image.Interface as I
import Graphics.Image.IO
import Graphics.Image.ColorSpace
#if defined(USE_Chart)  
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
#endif
import qualified Data.Colour as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)


-- | A single channel histogram of an image.
data Histogram = Histogram { hBins :: V.Vector Int
                             -- ^ Vector containing pixel counts. Index of a
                             -- vector serves as an original pixel value.
                           , hName :: String
                             -- ^ Name of the channel that will be displayed in
                             -- the legend.
                           , hColour :: C.AlphaColour Double
                             -- ^ Color of a plotted line.
                           }


-- | Create a histogram per channel with 256 bins each.
getHistograms :: forall arr cs e . (SequentialArray arr Gray e,
                                    SequentialArray arr cs e, Elevator e) =>
                 Image arr cs e
              -> [Histogram]
getHistograms = P.zipWith setCh (enumFrom (toEnum 0) :: [cs]) . P.map getHistogram . toGrayImages
  where setCh cs h = h { hName = show cs
                       , hColour = csColour cs }

-- | Generate a histogram with 256 bins for a single channel Gray image.
getHistogram :: (SequentialArray arr Gray e, Elevator e) =>
                Image arr Gray e
             -> Histogram
getHistogram img = Histogram { hBins = V.modify countBins $
                                       V.replicate
                                       (1 + fromIntegral (maxBound :: Word8)) (0 :: Int)
                             , hName = show Gray
                             , hColour = csColour Gray } where
  incBin v (toWord8 -> PixelGray g) = modify v (+1) $ fromIntegral g
  countBins v = I.mapM_ (incBin v) img
  

-- | Write histograms into a PNG image file.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeHistograms "images/frog_histogram.png" $ getHistograms frog
--
-- <<images/frog_histogram.png>>
--
writeHistograms :: FilePath -> [Histogram] -> IO ()
#if defined(USE_Chart)  
writeHistograms fileName hists =
  toFile def fileName $ do
    layout_title .= "Histogram"
    setColors $ P.map hColour hists
    let axis = set la_nTicks 20 . set la_nLabels 14
    layout_x_axis . laxis_generate .= scaledIntAxis (axis defaultIntAxis) (0, 260)
    P.mapM_ plotHist hists where
      plotHist h = plot (line (hName h) [V.toList $ V.imap (,) $ hBins h])
#else
writeHistograms _ _ =
  error "There is currently no backend that can plot histograms."
#endif

-- | Display image histograms using an external program. Works in a similar way as
-- `Graphics.Image.IO.displayImage`.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> displayHistograms $ getHistograms frog
--
displayHistograms :: [Histogram] -> IO ()
displayHistograms = displayHistogramsUsing defaultViewer False


-- | Display image histograms using an external program. Works in a similar way as
-- `Graphics.Image.IO.displayImageUsing`.
displayHistogramsUsing :: ExternalViewer
                       -> Bool
                       -> [Histogram] -> IO ()
displayHistogramsUsing viewer block hists = do
  let display = do
        tmpDir <- getTemporaryDirectory
        histPath <- (</> "tmp-hist.png") <$> createTempDirectory tmpDir "hip-histogram"
        writeHistograms histPath hists
        displayImageFile viewer histPath
  if block
    then display
    else forkIO display >> return ()



-- | Used for backwards compatibility with vector.
modify :: (PrimMonad m, V.Unbox a) => MV.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify v f idx = do
  e <- MV.read v idx
  MV.write v idx $ f e
