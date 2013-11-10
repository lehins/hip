{-# LANGUAGE TypeFamilies, TemplateHaskell, ViewPatterns, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Data.Image.Gray (
  Gray (..)
  ) where

import Data.Image.Internal
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Gray = Gray Double
          | GrayA Double Double deriving (Show)


instance Pixel Gray where
  data Image Gray = GrayImage (RepaImage Gray)
  
  width (GrayImage img) = rWidth img

  height (GrayImage img) = rHeight img

  ref (GrayImage img) x y = rRef img x y

  makeImage w h op = GrayImage $ rMakeImage w h op

  fromVector w h v = GrayImage $ rFromVector w h v

  toVector (GrayImage img) = rToVector img

  compute (GrayImage img) = GrayImage . rCompute $ img

yPxOp1 op (Gray y) = Gray (op y)
yPxOp1 op (GrayA y a) = GrayA (op y) (op a)

yPxOp op (Gray y1) (Gray y2) = Gray (op y1 y2)
yPxOp op (GrayA y1 a) (Gray y2) = GrayA (op y1 y2) a
yPxOp op (Gray y1) (GrayA y2 a) = GrayA (op y1 y2) a
yPxOp op (GrayA y1 a1) (GrayA y2 a2) = GrayA (op y1 y2) (op a1 a2)


instance Num Gray where
  (+) = yPxOp (+)
  (-) = yPxOp (-)
  (*) = yPxOp (*)
  abs = yPxOp1 abs
  signum = yPxOp1 signum
  fromInteger (fromIntegral -> n) = Gray n 


unboxGray (Gray y) = (y, Nothing)
unboxGray (GrayA y a) = (y, Just a)
boxGray (y, Nothing) = Gray y
boxGray (y, Just a) = GrayA y a

derivingUnbox "Gray"
    [t| (V.Unbox Double) => Gray -> (Double, Maybe Double) |]
    [| unboxGray |]
    [| boxGray |]

