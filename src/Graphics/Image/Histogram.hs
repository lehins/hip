{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Histogram (
  Histogram(..), getHistograms, getHistogram
  ) where

import Prelude hiding (map, mapM_)
import qualified Prelude as P (map)
import Control.Monad.Primitive (PrimMonad (..))
import Graphics.Image.Interface
import Graphics.Image.ColorSpace
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data Histogram cs = Histogram { hBins :: V.Vector Int,
                                hChannel :: cs,
                                hName :: Maybe String }


getHistograms :: (SequentialArray arr cs Word8, SequentialArray arr Gray Word8) =>
                 Image arr cs Word8
              -> [Histogram cs]
getHistograms = P.map setCh . zip (enumFrom (toEnum 0)) . P.map getHistogram . toGrayImages
  where setCh (ch, h) = h { hChannel = ch }


getHistogram :: (SequentialArray arr Gray Word8) =>
                Image arr Gray Word8
             -> Histogram Gray
getHistogram img = Histogram { hBins = V.modify countBins $
                                       V.replicate (1 + fromIntegral (maxBound :: Word8)) 0
                             , hChannel = Gray
                             , hName = Nothing } where
  incBin v (PixelGray g) = modify v (+1) $ fromIntegral g
  countBins v = mapM_ (incBin v) img
  
             

modify :: (PrimMonad m, V.Unbox a) => MV.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify v f ix = do
  e <- MV.read v ix
  MV.write v ix $ f e
