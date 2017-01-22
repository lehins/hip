{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Image.IO.FormatsSpec
  ( spec ) where

import Prelude as P
import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.Property

import Graphics.Image as I

import Graphics.Image.InterfaceSpec()


data EncDec a b = EncDec a b deriving Show

instance Arbitrary (EncDec InputFormat OutputFormat) where
  arbitrary = elements $ fmap (uncurry EncDec)
              [ (InputBMP, OutputBMP)
              , (InputHDR, OutputHDR)
              , (InputPNG, OutputPNG)
              , (InputTIF, OutputTIF)
              , (InputTGA, OutputTGA)
              ]



prop_EncodeDecode :: Image VS RGB Double -> EncDec InputFormat OutputFormat -> Result
prop_EncodeDecode imgD (EncDec input output) =
  case decode input (BL.toStrict (encode output [] (I.map (fmap toDouble) imgWord8))) of
    Left err ->
      failed
      { reason = err
      }
    Right (img' :: Image VS RGB Double) ->
      let imgWord8' = I.map (fmap toWord8) img' in
      if imgWord8 == imgWord8'
        then succeeded
        else failed
             { reason =
               "Original img <" ++ show input ++ ">: \n" ++
               show (toLists imgWord8) ++
               "\n differes from decoded img: \n" ++ show (toLists imgWord8')
             }
    where imgWord8 = I.map (fmap toWord8) imgD
          

spec :: Spec
spec = do
  describe "Encode/Decode" $ do
    it "Input/Output Formats" $ property prop_EncodeDecode
