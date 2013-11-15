module Main where

import Data.Image.Base
import Data.Image.Color

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit ((@?=))

testsColor = [
  testGroup "Test Properties" [
     testProperty "plus" color_plus,
     testProperty "minus" color_minus
     ],
  testGroup "Test functions" [
    testCase "strongest" $ (strongest (RGB 1 2 3)) @?= fromIntegral 3
    ]
  ]

makeRGB (r,g,b) = RGB r g b

color_plus px1' px2' = (r1 + r2, g1 + g2, b1 + b2) == getRGB (px1 + px2)
  where types = (px1' :: (Double, Double, Double), px2' :: (Double, Double, Double))
        (r1, g1, b1) = px1'
        (r2, g2, b2) = px2'
        (px1, px2) = (makeRGB px1', makeRGB px2')

color_minus px1' px2' = (r1 - r2, g1 - g2, b1 - b2) == getRGB (px1 - px2)
  where types = (px1' :: (Double, Double, Double), px2' :: (Double, Double, Double))
        (r1, g1, b1) = px1'
        (r2, g2, b2) = px2'
        (px1, px2) = (makeRGB px1', makeRGB px2')



main = defaultMain $ testsColor
