{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances,
UndecidableInstances #-}

module Data.Image where

--base>=4
import Control.Applicative
import qualified Data.Complex as C
import Data.Maybe(fromJust)
import Data.Monoid

--bytestring-0.10.0.2
import qualified Data.ByteString.Char8 as B

import qualified Data.Vector as V

type PixelOp px = Int -> Int -> px


type Gray = Int

data Pixel = Binary
           | Gray
           | Color

instance Show Pixel where
  show Binary = "Binary"
  show Gray   = "Gray"
  show Color  = "Color"
  
class DigitalImage img px | px -> img where

  width :: img -> Int
  
  height :: img -> Int

  dim :: img -> (Int, Int)

  ref :: img -> Int -> Int -> px
  
  makeImage :: Int -> Int -> (PixelOp px) -> img


data Image px = Image { iWidth :: Int,
                        iHeight :: Int,
                        iPixels :: V.Vector px }


instance DigitalImage (Image px) px where
  
  width  i = iWidth i
  
  height i = iHeight i

  dim i = (iWidth i, iHeight i)

  -- (1,1) is first pixel
  ref i x y = (iPixels i) V.! ((iWidth i)*(y -1) + (x - 1))

  makeImage h w f = Image w h (V.generate (w*h) pxOp) where
    pxOp n = if h < 1 || w < 1 then error "Invalid dimensions" 
             else f x y where x = (n `mod` w) + 1
                              y = div (n - x + 1) w


instance Functor Image where
  fmap f (Image w h pixels) = Image w h (fmap f pixels)

instance Applicative Image where
  pure a = Image 1 1 $ V.singleton a
  (<*>) (Image w h partial) (Image w' h' toApply)
    | w /= w' && h /= h' = error "Cannot apply images of unequal dimensions."
    | otherwise = Image w h (V.imap func toApply) where
        func i e = (partial V.! i) e


instance Show (Image px) where
  show (Image w h pxs) =
    -- TODO add Pixel type printing
    "<Image " ++ (show w) ++ "x" ++ (show h) ++ ">"

