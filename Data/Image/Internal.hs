{-# LANGUAGE ViewPatterns, TypeFamilies, FlexibleContexts #-}

module Data.Image.Internal where

import Data.Image.Base
import Data.Array.Repa
import Prelude hiding (map, zipWith)
import qualified Data.Vector.Unboxed as V


data RepaImage px = VectorImage (Array U DIM2 px)
                  | DelayedImage (Array D DIM2 px)
                  | PureImage (Array D DIM0 px)

isSmall arr = w < 50 && h < 50 where (Z :. w  :. h) = extent arr

getInner (VectorImage arr) = delay arr
getInner (DelayedImage arr) = arr

liftI op (PureImage arr) = PureImage $ map op arr
liftI op img = DelayedImage $ map op $ getInner img

liftI2 op (PureImage arr) img2 =
  DelayedImage $ map (op (arr ! Z)) (getInner img2)
liftI2 op img1 (PureImage arr) =
  DelayedImage $ map (flip op (arr ! Z)) (getInner img1)
liftI2 op img1 img2 =
  DelayedImage $ zipWith op (getInner img1) (getInner img2)

{-
rFold :: Pixel px => (px -> px -> px) -> px -> RepaImage px -> px
rFold op px (getInner -> arr)
  | isSmall arr = foldAllS op px arr
  | otherwise = head $ foldAllP op px arr
-}


instance (V.Unbox px, Num px) => Num (RepaImage px) where
  (+) = liftI2 (+)
  (-) = liftI2 (-)
  (*) = liftI2 (*)
  abs = liftI abs
  signum = liftI signum
  fromInteger i = PureImage $ fromFunction Z (const . fromInteger $ i)

rWidth (VectorImage (extent -> (Z :. w :. _))) = w
rWidth (DelayedImage (extent -> (Z :. w :. _))) = w

rHeight (VectorImage (extent -> (Z :. _ :. h))) = h
rHeight (DelayedImage (extent -> (Z :. _ :. h))) = h


rRef (VectorImage arr) x y = index arr (Z :. x :. y)
rRef (DelayedImage arr) x y = index arr (Z :. x :. y)

rMakeImage w h f = DelayedImage . fromFunction (Z :. w :. h) $ g where
  g (Z :. x :. y) = f x y

rFromVector w h = VectorImage . (fromUnboxed (Z :. w :. h))

rToVector (rCompute -> (VectorImage arr)) = toUnboxed arr

rCompute i@(VectorImage _) = i
rCompute (DelayedImage arr)
  | isSmall arr = VectorImage parr
  | otherwise = VectorImage . computeUnboxedS $ arr
  where [parr] = computeUnboxedP arr


