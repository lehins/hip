{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Image.Processing.Canny
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Canny
  ( -- * Canny Edge Detection
    canny
  , cannyGeneral
  ) where

import Control.Applicative
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A
import Graphics.Image.Internal as I
import Graphics.Image.Processing.Binary
import Graphics.Image.Processing.Filter
import qualified Graphics.Pixel as CM
import Prelude as P
import GHC.Word
import System.IO.Unsafe
import Graphics.Image.IO


-- | Apply Canny edge detection with customized options to a grayscale image.
--
-- ====__Examples__
--
-- >>> img <- readImageY "images/frog.jpg"
-- >>> let cannyCustom = cannyGeneral gaussianBlur3x3 prewittHorizontal prewittVertical
-- >>> writeImageExact "images/doc/frog_cannyGeneral.jpg" $ cannyCustom 0.2 0.4 $ toImageGrayscale img
--
-- <<images/frog.jpg>> <<images/doc/frog_cannyGeneral.jpg>>
--
-- @since 2.0.0
cannyGeneral ::
     (Elevator e, RealFloat e)
  => (Border (Pixel X e) -> Image X e -> Image X e)
  -- ^ Blurring function. Usually a `gaussianBlur`
  -> Filter' X e
  -- ^ Horizontal gradient. Usually `sobelHorizontal`.
  -> Filter' X e
  -- ^ Vertical gradient. Usually `sobelVertical`.
  -> e -- ^ Low threshold in range @[0, 1]@
  -> e -- ^ High threshold in range @[0, 1]@
  -> Image X e
  -- ^ Source image. Only grayscale information from the image will be used.
  -> Image X Bit
cannyGeneral blur horizontalGrad verticalGrad threshLow threshHigh img =
  let blurred = blur Edge img
      magnitudeOrientation =
        gradientMagnitudeOrientation threshLow horizontalGrad verticalGrad blurred
      suppressed = suppress threshLow threshHigh magnitudeOrientation
  in hysteresis suppressed
{-# INLINE cannyGeneral #-}


-- | Apply Canny edge detection with common defaults of `gaussianBlur5x5` and
-- `sobelOperator` for the gradient.
--
-- ====__Examples__
--
-- >>> frog <- readImageY "images/frog.jpg"
-- >>> writeImageExact "images/doc/frog_canny.jpg" $ canny 0.2 0.4 frog
--
-- <<images/frog.jpg>> <<images/doc/frog_canny.jpg>>
--
-- @since 2.0.0
canny ::
     (ColorSpace cs i e, RealFloat e)
  => e -- ^ Low threshold in range @[0, 1]@
  -> e -- ^ High threshold in range @[0, 1]@
  -> Image cs e
  -> Image X Bit
canny low high =
  cannyGeneral gaussianBlur5x5 sobelHorizontal sobelVertical low high . I.map grayscalePixel
{-# INLINE canny #-}

orientationUndefined
  , orientationPositiveDiagonal
  , orientationVertical
  , orientationNegativeDiagonal
  , orientationHorizontal :: Word8
orientationUndefined        = 0
orientationPositiveDiagonal = 1
orientationVertical         = 2
orientationNegativeDiagonal = 3
orientationHorizontal       = 4

edgeNone, edgeWeak, edgeStrong :: Pixel X Word8
edgeNone   = 0
edgeWeak   = 128
edgeStrong = 255


gradientMagnitudeOrientation ::
     (Elevator e, RealFloat e)
  => e
  -> Filter' X e
  -- ^ Horizontal gradient
  -> Filter' X e
  -- ^ Vertical gradient
  -> Image X e
  -> A.Array A.U Ix2 (e, Word8)
gradientMagnitudeOrientation !threshLow horGrad vertGrad (Image arr) =
  A.compute $
  A.mapStencil
    Edge
    (liftA2
       (\(CM.PixelX x) (CM.PixelX y) -> (mag x y, orientation x y))
       (toStencil horGrad)
       (toStencil vertGrad))
    arr
  where
    !negThreshLow = negate threshLow
    mag x y = sqrt (x * x + y * y)
    {-# INLINE mag #-}
    orientation x y
      -- Don't bother computing orientation if vector is below threshold.
      | x >= negThreshLow
      , x < threshLow
      , y >= negThreshLow
      , y < threshLow = orientationUndefined
      | otherwise =
        let !d = atan2 y x
            -- Determine the angle of the vector and rotate it around a bit
            -- to make the segments easier to classify.
            !dRot = 4 * d / pi - 0.5
            -- Normalize angle to beween 0..8
            !dNorm =
              if dRot < 0
                then dRot + 8
                else dRot
                -- Doing explicit tests seems to be faster than using the FP floor function.
         in W8#
              (if dNorm >= 4
                 then if dNorm >= 6
                        then if dNorm >= 7
                               then 4## -- 7
                               else 3## -- 6
                        else if dNorm >= 5
                               then 2## -- 5
                               else 1## -- 4
                 else if dNorm >= 2
                        then if dNorm >= 3
                               then 4## -- 3
                               else 3## -- 2
                        else if dNorm >= 1
                               then 2## -- 1
                               else 1## -- 0
               )
    {-# INLINE orientation #-}
{-# INLINE gradientMagnitudeOrientation #-}



suppress :: (A.Unbox e, RealFloat e) => e -> e -> (Array A.U Ix2 (e, Word8)) -> Image X Word8
suppress !threshLow !threshHigh !magOrient =
  computeI $ A.mapStencil (Fill (0, 0)) (A.makeUnsafeStencil 3 1 comparePts) magOrient
  where
    {-# INLINE comparePts #-}
    comparePts _ getMag
      | o == orientationUndefined        = edgeNone
      | o == orientationHorizontal       = isMax (getMag (0 :. -1)) (getMag (0 :. 1))
      | o == orientationVertical         = isMax (getMag (-1 :. 0)) (getMag (1 :. 0))
      | o == orientationNegativeDiagonal = isMax (getMag (-1 :. 1)) (getMag (1 :. -1))
      | o == orientationPositiveDiagonal = isMax (getMag (-1 :. -1)) (getMag (1 :. 1))
      | otherwise = edgeNone
      where
        (!m, !o) = getMag (0 :. 0)
        {-# INLINE isMax #-}
        isMax intensity1 intensity2
          | m < threshLow = edgeNone
          | m < fst intensity1 = edgeNone
          | m < fst intensity2 = edgeNone
          | m < threshHigh = edgeWeak
          | otherwise = edgeStrong
{-# INLINE suppress #-}


-- | Select indices of strong edges.
selectStrong :: Image X Word8 -> Array S Ix1 Ix1
selectStrong =
  A.compute .
  A.simapMaybe
    (\ !ix !e ->
       if e == edgeStrong
         then Just ix
         else Nothing) .
  A.flatten .
  unImage
{-# INLINE selectStrong #-}



hysteresis ::
     Image X Word8 -- ^ Image with strong and weak edges set.
  -> Image X Bit
hysteresis img@(Image arr) = unsafePerformIO $ do
  let !strong = selectStrong img
      !szStrong = A.size strong
  -- 1. put all the strong edges in a stack
  vStack <- A.unsafeNew (Sz lenImg)
  -- TODO: wait for https://github.com/lehins/massiv/issues/103 to get implemened and
  -- optimize selectStrong to be loaded directly into vStack
  A.unsafeArrayLinearCopy strong 0 vStack 0 szStrong
  edges <- A.newMArray sz 0
  burn edges vStack (unSz szStrong)
  Image <$> A.unsafeFreeze (A.getComp arr) edges
  where
    !sz = A.size arr
    !lenImg = totalElem sz
    burn ::
         A.MArray A.RealWorld S Ix2 (Pixel X Bit)
      -> A.MArray A.RealWorld S Ix1 Ix1
      -> Int
      -> IO ()
    burn !edges !vStack = go
      where
        push :: Ix2 -> Int -> IO Int
        push !ix !top =
          case A.indexM arr ix of
            Nothing -> pure top
            Just src -> do
              dst <- A.unsafeRead edges ix
              if dst == off && src == edgeWeak
                -- Rescue the weak edge and push onto the stack
                then (top + 1) <$ A.unsafeWrite vStack top (toLinearIndex sz ix)
                else pure top
        {-# INLINE push #-}
        go !top
          | top == 0 = return ()
          | otherwise = do
            let !top' = top - 1
            -- Pop the first strong edge off the stack, look at all its neighbours, and
            -- if any of these neighbours is a weak edge we rescue it by labelling it as
            -- strong and adding it to the stack
            i <- A.unsafeLinearRead vStack top'
            let (y :. x) = fromLinearIndex sz i
            A.unsafeLinearWrite edges i on
            push   (y - 1 :. x - 1) top' >>=
              push (y - 1 :. x    ) >>=
              push (y - 1 :. x + 1) >>=
              push (y     :. x - 1) >>=
              push (y     :. x + 1) >>=
              push (y + 1 :. x - 1) >>=
              push (y + 1 :. x    ) >>=
              push (y + 1 :. x + 1) >>=
              go
{-# INLINE hysteresis #-}
