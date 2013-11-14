{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, InstanceSigs #-}

module Data.Image.Internal (
  Pixel (..), PixelOp,
  RealPixel(..),
  RepaImage(..),
  --Internal,
  rWidth, rHeight, rRef, rMakeImage, rFromVector, rToVector, rCompute
  ) where

import Prelude hiding ((++))
import Data.Array.Repa

import Data.Functor
import Data.Maybe
import Data.Default
import Data.Vector.Unboxed.Deriving
import Control.Applicative
import Data.Monoid
import qualified Data.List as L ((++))
import qualified Data.Vector.Unboxed as V

--type family Internal px :: *

type PixelOp px = Int -> Int -> px

class (V.Unbox px, Floating px, Fractional px, Num px, Eq px, Show px) =>
      Pixel px where
  data Image px :: *

  liftPx :: (Double -> Double) -> px -> px

  liftPx2 :: (Double -> Double -> Double) -> px -> px -> px

  width :: Image px -> Int

  height :: Image px -> Int

  ref :: Image px -> Int -> Int -> px

  makeImage :: Int -> Int -> PixelOp px -> Image px

  fromVector :: Int -> Int -> V.Vector px -> Image px

  toVector :: Image px -> V.Vector px

  compute :: Image px -> Image px

  singleton :: px -> Image px
  singleton px = makeImage 1 1 (\_ _ -> px)

  imageMap :: (px -> px) -> Image px -> Image px
  imageMap op img = makeImage w h f where 
    (w, h) = (width img, height img)
    f x y = op (ref img x y)

  imageApply :: (px -> px -> px) -> Image px -> Image px -> Image px 
  imageApply op img1 img2 = makeImage w h f where
    (w, h) = (width img1, height img1)
    f x y = op (ref img1 x y) (ref img2 x y)

instance Pixel px => Num (Image px) where
  (+) = imageApply (+)
  (-) = imageApply (-)
  (*) = imageApply (*)
  abs = imageMap abs
  signum = imageMap signum
  fromInteger = singleton . fromInteger
  
instance Pixel px => Fractional (Image px) where
  (/) = imageApply (/)
  fromRational = singleton . fromRational

instance Pixel px => Floating (Image px) where
  pi      = singleton pi
  exp     = imageMap exp
  log     = imageMap log
  sin     = imageMap sin
  cos     = imageMap cos
  asin    = imageMap asin
  atan    = imageMap atan
  acos    = imageMap acos
  sinh    = imageMap sinh
  cosh    = imageMap cosh
  asinh   = imageMap asinh
  atanh   = imageMap atanh
  acosh   = imageMap acosh


class Pixel px => RealPixel px where
  -- Suppose to produce 0 upon division by zero. Necessary for Complex px
  safeDiv :: px -> px -> px
  fromDouble :: Double -> px

data RepaImage px = UnboxedImage (Array U DIM2 px)
                  | DelayedImage (Array D DIM2 px)
                  | PureImage (Array D DIM0 px)

instance Functor RepaImage where
  fmap = fmap

instance Applicative RepaImage where
  pure a = PureImage $ fromFunction Z (const a)
  (<*>) (PureImage arr1) (PureImage arr2) = pure $ (arr1 ! Z) (arr2 ! Z)
  (<*>) (DelayedImage arr@(toFunction -> (sh, f))) (PureImage arr2) =
    DelayedImage $ fromFunction sh f' where
      toApply = arr2 ! Z
      f' sh1 = (f sh1) toApply
  (<*>) (DelayedImage arr1@(toFunction -> (sh1, f1)))
    (DelayedImage arr2@(toFunction -> (sh2, f2)))
      | sh1 /= sh2 = error "Dimensions must agree"
      | otherwise = DelayedImage $ fromFunction sh1 f' where
         f' sh = (f1 sh) (f2 sh)

instance Pixel px => Num (RepaImage px) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i

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


-- resolving conflict with ++ from Prelude
(++:) = (L.++)

--instance Applicative Image where
--  pure a = PureImage a

--iImgOp op img1 img2 = fmap op img
{-
instance Pixel px => Num (Image px) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  --fromInteger i = pure $ fromInteger i
-}
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

