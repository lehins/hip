--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where

import Graphics.Image.ProcessingSpec as ProcessingSpec
import Graphics.Image.Processing.BinarySpec as BinarySpec
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Test.Hspec


-- | Main entry point. Returns ExitFailure if a test fails.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec $ do
    describe "Processing" $ do
      ProcessingSpec.spec
    describe "Binary" $ do
      BinarySpec.spec
