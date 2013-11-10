{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, MultiParamTypeClasses #-}

module Data.Image.Internal (
  Pixel (..), PixelOp,
  RepaImage(..),
  rWidth, rHeight, rRef, rMakeImage, rFromVector, rToVector, rCompute
  ) where

import Prelude hiding ((++))
import Data.Array.Repa
import Data.Functor
import Data.Maybe
import Data.Default
import Data.Vector.Unboxed.Deriving
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V


type PixelOp px = Int -> Int -> px

class (V.Unbox px, Num px) => Pixel px where
  data Image px :: *

  width :: Image px -> Int

  height :: Image px -> Int

  ref :: Image px -> Int -> Int -> px

  makeImage :: Int -> Int -> PixelOp px -> Image px

  fromVector :: Int -> Int -> V.Vector px -> Image px

  toVector :: Image px -> V.Vector px

  compute :: Image px -> Image px


data RepaImage px = UnboxedImage (Array U DIM2 px)
                  | DelayedImage (Array D DIM2 px)

rWidth (UnboxedImage (extent -> (Z :. w :. _))) = w
rWidth (DelayedImage (extent -> (Z :. w :. _))) = w

rHeight (UnboxedImage (extent -> (Z :. _ :. h))) = h
rHeight (DelayedImage (extent -> (Z :. _ :. h))) = h


rRef (UnboxedImage arr) x y = index arr (Z :. x :. y)
rRef (DelayedImage arr) x y = index arr (Z :. x :. y)

rMakeImage w h f = DelayedImage $ fromFunction (Z :. w :. h) g where
  g (Z :. x :. y) = f x y

rFromVector w h = UnboxedImage . (fromUnboxed (Z :. w :. h))

rToVector (rCompute -> (UnboxedImage arr)) = toUnboxed arr

rCompute i@(UnboxedImage _) = i
rCompute (DelayedImage arr) = UnboxedImage uarr where
  [uarr] = computeUnboxedP arr

-- resolving conflict with 
(++:) = (L.++)

instance Functor Image where
  fmap = fmap

derivingUnbox "Maybe"
    [t| (Default a, V.Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]


{-
data VectorImage px = VectorImage { vWidth :: Int,
                                    vHeight :: Int,
                                    vPixels :: V.Vector px }



vRef i x y = vPixels i V.! (vWidth i * y + x)

vMake w h f = VectorImage w h (V.generate (w*h) vOp) where
  vOp n = if h < 1 || w < 1
          then error $ "Invalid dimensions: "++:show w++:"x"++:show h
          else f x y where x = n `mod` w
                           y = (n - x) `div` w

vFromVector = VectorImage 

-}

{-
instance Show (VectorImage px) where
  show img = "<Image : "++:show (vWidth img)++:"x"++:show (vHeight img)++:">"
-}
{-
instance Functor VectorImage where
  fmap f (VectorImage width height pixels) = VectorImage width height (fmap f pixels)
-}

