{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Interface.Repa
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa (
  -- * Conversion
  fromRepaArrayS, fromRepaArrayP, toRepaArray,
  -- * Representation
  RS(..), RP(..),
  ) where

import Graphics.Image.Interface.Repa.Internal


