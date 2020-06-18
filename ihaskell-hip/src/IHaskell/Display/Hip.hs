{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : IHaskell.Display.Hip
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module IHaskell.Display.Hip
  ( module IHaskell.Display
  , module I
  ) where

import Data.List.NonEmpty
import Data.Default.Class (def)
import qualified Data.Massiv.Array.IO as M
import Data.Word
import qualified Graphics.Image as I

import Data.ByteString.Lazy (toStrict)
import IHaskell.Display (Base64, Display(..), DisplayData, Height,
                         IHaskellDisplay(display), Width, base64, gif, jpg, png)

instance IHaskellDisplay (I.Image (M.SRGB 'I.NonLinear) Word8) where
  display = base64encode png M.PNG

instance IHaskellDisplay (I.Image (M.SRGB 'I.NonLinear) Word16) where
  display = base64encode png M.PNG

instance IHaskellDisplay (I.Image (M.YCbCr (M.SRGB 'I.NonLinear)) Word8) where
  display = base64encode jpg M.JPG

instance IHaskellDisplay (I.Image (M.CMYK (M.SRGB 'I.NonLinear)) Word8) where
  display = base64encode jpg M.JPG

instance IHaskellDisplay (I.Image M.Y' Word8) where
  display = base64encode png M.PNG

instance IHaskellDisplay (I.Image M.Y' Word16) where
  display = base64encode png M.PNG

instance IHaskellDisplay (I.Image (I.Alpha M.Y') Word8) where
  display = base64encode png M.PNG

instance IHaskellDisplay (I.Image (I.Alpha M.Y') Word16) where
  display = base64encode png M.PNG

instance {-# OVERLAPPABLE #-} (M.ColorSpace cs i e, M.ColorSpace (M.BaseSpace cs) i e) =>
                              IHaskellDisplay (I.Image cs e) where
  display = base64encode png (M.Auto M.PNG)



base64encode ::
     (M.Writable f (M.Image I.S cs e), M.ColorModel cs e)
  => (Width -> Height -> Base64 -> DisplayData)
  -> f
  -> I.Image cs e
  -> IO Display
base64encode toDisplayData format img@(I.Image arr) = do
  let I.Sz2 m n = I.dims img
  bs <- M.encodeM format def arr
  pure $ Display [toDisplayData n m $ base64 $ toStrict bs]
