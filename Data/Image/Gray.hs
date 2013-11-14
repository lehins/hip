{-# LANGUAGE TypeFamilies, TemplateHaskell, ViewPatterns, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Data.Image.Gray (
  Gray (..)
  ) where

import Data.Image.Base
import Data.Image.Internal
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Gray = Gray Double
          | GrayA Double Double deriving Eq

instance Pixel Gray where
  data Image Gray = GrayImage (RepaImage Gray)
  
  liftPx f (Gray y) = Gray (f y)
  liftPx f (GrayA y a) = GrayA (f y) (f a)
  
  liftPx2 f (Gray y1) (Gray y2) = Gray (f y1 y2)
  liftPx2 f (GrayA y1 a) (Gray y2) = GrayA (f y1 y2) a
  liftPx2 f (Gray y1) (GrayA y2 a) = GrayA (f y1 y2) a
  liftPx2 f (GrayA y1 a1) (GrayA y2 a2) = GrayA (f y1 y2) (f a1 a2)
    
  width (GrayImage img) = rWidth img

  height (GrayImage img) = rHeight img

  ref (GrayImage img) x y = rRef img x y

  makeImage w h op = GrayImage $ rMakeImage w h op

  imageMap op (GrayImage img) = GrayImage $ liftI op img

  imageZipWith op (GrayImage img1) (GrayImage img2) =
    GrayImage $ liftI2 op img1 img2

  fromVector w h v = GrayImage $ rFromVector w h v

  toVector (GrayImage img) = rToVector img

  compute (GrayImage img) = GrayImage . rCompute $ img

instance Num Gray where
  (+)           = liftPx2 (+)
  (-)           = liftPx2 (-)
  (*)           = liftPx2 (*)
  abs           = liftPx abs
  signum        = liftPx signum
  fromInteger n = Gray . fromIntegral $ n 

instance Fractional Gray where
  (/)          = liftPx2 (/)
  recip        = liftPx recip
  fromRational = Gray . fromRational

instance Floating Gray where
  pi      = Gray pi
  exp     = liftPx exp
  log     = liftPx log
  sin     = liftPx sin
  cos     = liftPx cos
  asin    = liftPx asin
  atan    = liftPx atan
  acos    = liftPx acos
  sinh    = liftPx sinh
  cosh    = liftPx cosh
  asinh   = liftPx asinh
  atanh   = liftPx atanh
  acosh   = liftPx acosh

instance Show Gray where
  show (Gray y) = "<Gray:("++show y++")>"
  show (GrayA y a) = "<GrayA:("++show y++"|"++show a++")>"


unboxGray (Gray y) = (y, Nothing)
unboxGray (GrayA y a) = (y, Just a)
boxGray (y, Nothing) = Gray y
boxGray (y, Just a) = GrayA y a

derivingUnbox "GrayPixel"
    [t| (V.Unbox Double) => Gray -> (Double, Maybe Double) |]
    [| unboxGray |]
    [| boxGray |]

