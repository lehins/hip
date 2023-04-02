{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Image.IO.Histogram
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Histogram (
  Histogram(..), Histograms, ChannelColour(..), getHistograms, getHistogram,
  equalizeHistogram, cdf,
  displayHistograms, writeHistograms
  ) where

import           Control.Concurrent                        (forkIO)
import           Control.Monad                             (void)
import qualified Data.Colour                               as C
import qualified Data.Colour.Names                         as C
import qualified Data.Vector.Unboxed                       as V
import           Prelude                                   as P
import           System.Directory                          (getTemporaryDirectory)
import           System.FilePath                           ((</>))
import           System.IO.Temp                            (createTempDirectory)

import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface                  as I
import           Graphics.Image.IO
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

#if MIN_VERSION_vector(0,11,0)
import           Data.Vector.Unboxed.Mutable               (modify)
#else
import           Control.Monad.Primitive                   (PrimMonad (..))
import qualified Data.Vector.Unboxed.Mutable               as MV

modify :: (PrimMonad m, V.Unbox a) => MV.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify v f idx = do
  e <- MV.read v idx
  MV.write v idx $ f e
#endif


class ChannelColour cs where

  -- | Get a pure colour representation of a channel.
  csColour :: cs -> C.AlphaColour Double


-- | A single channel histogram of an image.
data Histogram = Histogram { hBins   :: V.Vector Int
                             -- ^ Vector containing pixel counts. Index of a
                             -- vector serves as an original pixel value.
                           , hName   :: String
                             -- ^ Name of the channel that will be displayed in
                             -- the legend.
                           , hColour :: C.AlphaColour Double
                             -- ^ Color of a plotted line.
                           }
-- | For now it is just a type synonym, but in the future it might become a custom
-- data type with fields like title, width, height, etc.
type Histograms = [Histogram]


-- | Create a histogram per channel with 256 bins each.
getHistograms :: forall arr cs e . (ChannelColour cs, MArray arr X e, Array arr X e,
                                    MArray arr cs e, Array arr cs e) =>
                 Image arr cs e
              -> Histograms
getHistograms = P.zipWith setCh (enumFrom (toEnum 0) :: [cs]) . P.map getHistogram . toImagesX
  where setCh !cs !h = h { hName = show cs
                         , hColour = csColour cs }
        {-# INLINE setCh #-}
{-# INLINE getHistograms #-}

-- | Generate a histogram with 256 bins for a single channel Gray image.
getHistogram :: MArray arr X e =>
                Image arr X e
             -> Histogram
getHistogram !img = Histogram { hBins = V.modify countBins $
                                        V.replicate
                                        (1 + fromIntegral (maxBound :: Word8)) (0 :: Int)
                              , hName = show X
                              , hColour = csColour X }
  where
    incBin !v (PixelX x) = modify v (+1) $ fromIntegral (toWord8 x)
    {-# INLINE incBin #-}
    countBins !v = I.mapM_ (incBin v) img
    {-# INLINE countBins #-}
{-# INLINE getHistogram #-}


-- | Discrete cumulative distribution function.
cdf :: MArray arr X e => Image arr X e -> V.Vector Double
cdf !img = V.unfoldr gen (0, 0)
  where
    !p = 1 / fromIntegral (m * n)
    !(m, n) = dims img
    !histogram = hBins $ getHistogram img
    !l = V.length histogram
    gen !(acc, k)
      | k == l = Nothing
      | otherwise =
        let !acc' = acc + fromIntegral (V.unsafeIndex histogram k)
        in Just (acc' * p, (acc', k + 1))
    {-# INLINE gen #-}
{-# INLINE cdf #-}


-- | Converts an image to Luma and performs histogram equalization.
equalizeHistogram
  :: ( ToY cs e
     , Array arr cs e
     , Array arr Y Double
     , Array arr X Word8
     , MArray arr X Word8
     )
  => Image arr cs e -> Image arr Y Double
equalizeHistogram !img = I.map f imgX where
  !imgX = I.map (toX . toPixelY) img
  !cdfImg = cdf imgX
  toX (PixelY y) = PixelX $ toWord8 y
  {-# INLINE toX #-}
  f (PixelX x) = PixelY (V.unsafeIndex cdfImg (fromIntegral x))
  {-# INLINE f #-}
{-# INLINE equalizeHistogram #-}


-- | Write histograms into a PNG image file.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> writeHistograms "images/frog_histogram.svg" $ getHistograms frog
--
-- <<images/frog_histogram.svg>>
--
writeHistograms :: FilePath -> Histograms -> IO ()
writeHistograms fileName hists =
  toFile def fileName $ do
    layout_title .= "Histogram"
    setColors $ P.map hColour hists
    let axis = set la_nTicks 20 . set la_nLabels 14
    layout_x_axis . laxis_generate .= scaledIntAxis (axis defaultIntAxis) (0, 260)
    let plotHist h = plot $ line (hName h) [V.toList . V.imap (,) $ hBins h]
    P.mapM_ plotHist hists

-- | Display image histograms using an external program. Works in a similar way as
-- `Graphics.Image.IO.displayImage`.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> displayHistograms $ getHistograms frog
--
displayHistograms :: Histograms -> IO ()
displayHistograms = displayHistogramsUsing defaultViewer False


-- | Display image histograms using an external program. Works in a similar way as
-- `Graphics.Image.IO.displayImageUsing`.
displayHistogramsUsing :: ExternalViewer
                       -> Bool
                       -> Histograms -> IO ()
displayHistogramsUsing viewer block hists = do
  let display = do
        tmpDir <- getTemporaryDirectory
        histPath <- fmap (</> "tmp-hist.svg") (createTempDirectory tmpDir "hip-histogram")
        writeHistograms histPath hists
        displayImageFile viewer histPath
  if block
    then display
    else void $ forkIO display

instance ChannelColour X where
  csColour _ = C.opaque C.darkgray

instance ChannelColour Y where
  csColour _ = C.opaque C.darkgray

instance ChannelColour YA where
  csColour LumaYA  = csColour LumaY
  csColour AlphaYA = C.opaque C.gray


instance ChannelColour RGB where
  csColour RedRGB   = C.opaque C.red
  csColour GreenRGB = C.opaque C.green
  csColour BlueRGB  = C.opaque C.blue

instance ChannelColour RGBA where
  csColour RedRGBA   = C.opaque C.red
  csColour GreenRGBA = C.opaque C.green
  csColour BlueRGBA  = C.opaque C.blue
  csColour AlphaRGBA = C.opaque C.gray


instance ChannelColour HSI where
  csColour HueHSI = C.opaque C.purple
  csColour SatHSI = C.opaque C.orange
  csColour IntHSI = C.opaque C.darkblue

instance ChannelColour HSIA where
  csColour HueHSIA   = C.opaque C.purple
  csColour SatHSIA   = C.opaque C.orange
  csColour IntHSIA   = C.opaque C.darkblue
  csColour AlphaHSIA = C.opaque C.gray


instance ChannelColour CMYK where
  csColour CyanCMYK = C.opaque C.cyan
  csColour MagCMYK  = C.opaque C.magenta
  csColour YelCMYK  = C.opaque C.yellow
  csColour KeyCMYK  = C.opaque C.black

instance ChannelColour CMYKA where
  csColour CyanCMYKA  = csColour CyanCMYK
  csColour MagCMYKA   = csColour MagCMYK
  csColour YelCMYKA   = csColour YelCMYK
  csColour KeyCMYKA   = csColour KeyCMYK
  csColour AlphaCMYKA = C.opaque C.grey


instance ChannelColour YCbCr where
  csColour LumaYCbCr  = C.opaque C.darkgray
  csColour CBlueYCbCr = C.opaque C.darkblue
  csColour CRedYCbCr  = C.opaque C.darkred


instance ChannelColour YCbCrA where
  csColour LumaYCbCrA  = csColour LumaYCbCr
  csColour CBlueYCbCrA = csColour CBlueYCbCr
  csColour CRedYCbCrA  = csColour CRedYCbCr
  csColour AlphaYCbCrA = C.opaque C.gray
