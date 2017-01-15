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
  VU(..), VS(..), RSU(..), RPU(..), RSS(..), RPS(..)
  ) where


import Graphics.Image.ColorSpace
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector (VU(..), VS(..))
import Graphics.Image.Interface.Repa (RSU(..), RPU(..), RSS(..), RPS(..))
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
"Image RPU Y Double ^ 2/Int" forall (img :: Image RPU Y Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RPU Y Double ^ 3/Int" forall (img :: Image RPU Y Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RPU Y Double ^ 4/Int" forall (img :: Image RPU Y Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RPU Y Double ^ 5/Int" forall (img :: Image RPU Y Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RPU Y Double ^ 2/Integer" forall (img :: Image RPU Y Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RPU Y Double ^ 3/Integer" forall (img :: Image RPU Y Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RPU Y Double ^ 4/Integer" forall (img :: Image RPU Y Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RPU Y Double ^ 5/Integer" forall (img :: Image RPU Y Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RPU Y Double ^ n" forall (img :: Image RPU Y Double) n. img ^ n = I.map (^n) img
"Image RSU Y Double ^ 2/Int" forall (img :: Image RSU Y Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RSU Y Double ^ 3/Int" forall (img :: Image RSU Y Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RSU Y Double ^ 4/Int" forall (img :: Image RSU Y Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RSU Y Double ^ 5/Int" forall (img :: Image RSU Y Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RSU Y Double ^ 2/Integer" forall (img :: Image RSU Y Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RSU Y Double ^ 3/Integer" forall (img :: Image RSU Y Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RSU Y Double ^ 4/Integer" forall (img :: Image RSU Y Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RSU Y Double ^ 5/Integer" forall (img :: Image RSU Y Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RSU Y Double ^ n" forall (img :: Image RSU Y Double) n. img ^ n = I.map (^n) img
"Image VU RGB Double ^ 2/Int" forall (img :: Image VU RGB Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image VU RGB Double ^ 3/Int" forall (img :: Image VU RGB Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image VU RGB Double ^ 4/Int" forall (img :: Image VU RGB Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image VU RGB Double ^ 5/Int" forall (img :: Image VU RGB Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image VU RGB Double ^ 2/Integer" forall (img :: Image VU RGB Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image VU RGB Double ^ 3/Integer" forall (img :: Image VU RGB Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image VU RGB Double ^ 4/Integer" forall (img :: Image VU RGB Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image VU RGB Double ^ 5/Integer" forall (img :: Image VU RGB Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image VU RGB Double ^ n" forall (img :: Image VU RGB Double) n. img ^ n = I.map (^n) img
"Image RPU RGB Double ^ 2/Int" forall (img :: Image RPU RGB Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RPU RGB Double ^ 3/Int" forall (img :: Image RPU RGB Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RPU RGB Double ^ 4/Int" forall (img :: Image RPU RGB Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RPU RGB Double ^ 5/Int" forall (img :: Image RPU RGB Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RPU RGB Double ^ 2/Integer" forall (img :: Image RPU RGB Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RPU RGB Double ^ 3/Integer" forall (img :: Image RPU RGB Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RPU RGB Double ^ 4/Integer" forall (img :: Image RPU RGB Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RPU RGB Double ^ 5/Integer" forall (img :: Image RPU RGB Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RPU RGB Double ^ n" forall (img :: Image RPU RGB Double) n. img ^ n = I.map (^n) img
"Image RSU RGB Double ^ 2/Int" forall (img :: Image RSU RGB Double). img ^ (2 :: Int) = I.map (^ (2 :: Int)) img
"Image RSU RGB Double ^ 3/Int" forall (img :: Image RSU RGB Double). img ^ (3 :: Int) = I.map (^ (3 :: Int)) img
"Image RSU RGB Double ^ 4/Int" forall (img :: Image RSU RGB Double). img ^ (4 :: Int) = I.map (^ (4 :: Int)) img
"Image RSU RGB Double ^ 5/Int" forall (img :: Image RSU RGB Double). img ^ (5 :: Int) = I.map (^ (5 :: Int)) img
"Image RSU RGB Double ^ 2/Integer" forall (img :: Image RSU RGB Double). img ^ (2 :: Integer) = I.map (^ (2 :: Integer)) img
"Image RSU RGB Double ^ 3/Integer" forall (img :: Image RSU RGB Double). img ^ (3 :: Integer) = I.map (^ (3 :: Integer)) img
"Image RSU RGB Double ^ 4/Integer" forall (img :: Image RSU RGB Double). img ^ (4 :: Integer) = I.map (^ (4 :: Integer)) img
"Image RSU RGB Double ^ 5/Integer" forall (img :: Image RSU RGB Double). img ^ (5 :: Integer) = I.map (^ (5 :: Integer)) img
"Image RSU RGB Double ^ n" forall (img :: Image RSU RGB Double) n. img ^ n = I.map (^n) img
 #-}



--{-# RULES
--"I.map/id" forall img. I.map id img = img
-- #-}
