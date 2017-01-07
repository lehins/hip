{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Graphics.Image.Types
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Types (
  module Graphics.Image.ColorSpace,
  module Graphics.Image.IO.Formats,
  Array, Image, MArray, MImage,
  Exchangable, Border(..),
  VU(..), VS(..), RS(..), RP(..),
  ) where


import Graphics.Image.ColorSpace
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector (VU(..), VS(..))
import Graphics.Image.Interface.Repa (RS(..), RP(..))
import Graphics.Image.IO.Formats



{-# RULES
"Image VU Y Double ^ 2/Int" forall (img :: Image VU Y Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image VU Y Double ^ 3/Int" forall (img :: Image VU Y Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image VU Y Double ^ 4/Int" forall (img :: Image VU Y Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image VU Y Double ^ 5/Int" forall (img :: Image VU Y Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image VU Y Double ^ 2/Integer" forall (img :: Image VU Y Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image VU Y Double ^ 3/Integer" forall (img :: Image VU Y Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image VU Y Double ^ 4/Integer" forall (img :: Image VU Y Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image VU Y Double ^ 5/Integer" forall (img :: Image VU Y Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image VU Y Double ^ n" forall (img :: Image VU Y Double) n. img ^ n = I.map (^n) img
"Image RP Y Double ^ 2/Int" forall (img :: Image RP Y Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RP Y Double ^ 3/Int" forall (img :: Image RP Y Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RP Y Double ^ 4/Int" forall (img :: Image RP Y Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RP Y Double ^ 5/Int" forall (img :: Image RP Y Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RP Y Double ^ 2/Integer" forall (img :: Image RP Y Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RP Y Double ^ 3/Integer" forall (img :: Image RP Y Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RP Y Double ^ 4/Integer" forall (img :: Image RP Y Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RP Y Double ^ 5/Integer" forall (img :: Image RP Y Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RP Y Double ^ n" forall (img :: Image RP Y Double) n. img ^ n = I.map (^n) img
"Image RS Y Double ^ 2/Int" forall (img :: Image RS Y Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RS Y Double ^ 3/Int" forall (img :: Image RS Y Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RS Y Double ^ 4/Int" forall (img :: Image RS Y Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RS Y Double ^ 5/Int" forall (img :: Image RS Y Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RS Y Double ^ 2/Integer" forall (img :: Image RS Y Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RS Y Double ^ 3/Integer" forall (img :: Image RS Y Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RS Y Double ^ 4/Integer" forall (img :: Image RS Y Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RS Y Double ^ 5/Integer" forall (img :: Image RS Y Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RS Y Double ^ n" forall (img :: Image RS Y Double) n. img ^ n = I.map (^n) img
"Image VU RGB Double ^ 2/Int" forall (img :: Image VU RGB Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image VU RGB Double ^ 3/Int" forall (img :: Image VU RGB Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image VU RGB Double ^ 4/Int" forall (img :: Image VU RGB Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image VU RGB Double ^ 5/Int" forall (img :: Image VU RGB Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image VU RGB Double ^ 2/Integer" forall (img :: Image VU RGB Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image VU RGB Double ^ 3/Integer" forall (img :: Image VU RGB Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image VU RGB Double ^ 4/Integer" forall (img :: Image VU RGB Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image VU RGB Double ^ 5/Integer" forall (img :: Image VU RGB Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image VU RGB Double ^ n" forall (img :: Image VU RGB Double) n. img ^ n = I.map (^n) img
"Image RP RGB Double ^ 2/Int" forall (img :: Image RP RGB Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RP RGB Double ^ 3/Int" forall (img :: Image RP RGB Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RP RGB Double ^ 4/Int" forall (img :: Image RP RGB Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RP RGB Double ^ 5/Int" forall (img :: Image RP RGB Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RP RGB Double ^ 2/Integer" forall (img :: Image RP RGB Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RP RGB Double ^ 3/Integer" forall (img :: Image RP RGB Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RP RGB Double ^ 4/Integer" forall (img :: Image RP RGB Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RP RGB Double ^ 5/Integer" forall (img :: Image RP RGB Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RP RGB Double ^ n" forall (img :: Image RP RGB Double) n. img ^ n = I.map (^n) img
"Image RS RGB Double ^ 2/Int" forall (img :: Image RS RGB Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RS RGB Double ^ 3/Int" forall (img :: Image RS RGB Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RS RGB Double ^ 4/Int" forall (img :: Image RS RGB Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RS RGB Double ^ 5/Int" forall (img :: Image RS RGB Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RS RGB Double ^ 2/Integer" forall (img :: Image RS RGB Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RS RGB Double ^ 3/Integer" forall (img :: Image RS RGB Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RS RGB Double ^ 4/Integer" forall (img :: Image RS RGB Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RS RGB Double ^ 5/Integer" forall (img :: Image RS RGB Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RS RGB Double ^ n" forall (img :: Image RS RGB Double) n. img ^ n = I.map (^n) img
 #-}
