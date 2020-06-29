{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Processing.Binary
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Binary
  ( -- * Construction
    on
  , off
  , isOn
  , isOff
  , module Graphics.Color.Algebra.Binary
    -- * Thresholding
  , threshold
  , threshold2
  , thresholdWith
  , thresholdWith2
  , Thresholding
  -- ** Common comparators
  , (!==!)
  , (!/=!)
  , (!<!)
  , (!<=!)
  , (!>!)
  , (!>=!)
  , (!&&!)
  , (!||!)
  , (.==.)
  , (./=.)
  , (.<.)
  , (.<=.)
  , (.>.)
  , (.>=.)
  , (.&&.)
  , (.||.)
  -- * Bitwise operations
  , or
  , and
  , invert
  , disjunction
  , conjunction
  -- * Binary Morphology
  -- $morphology
  , erode
  , dialate
  , open
  , close
  ) where

import Control.Applicative
import Data.Bits
import qualified Data.Foldable as F
import Data.Monoid (All(..), Any(..))
import Graphics.Color.Algebra.Binary
import Graphics.Color.Model as Model
import Graphics.Image.Internal as I
import Graphics.Image.Processing.Convolution
import Graphics.Pixel
import Prelude as P hiding (and, or)

infix  4  .==., ./=., .<., .<=., .>=., .>., !==!, !/=!, !<!, !<=!, !>=!, !>!
infixr 3  .&&., !&&!
infixr 2  .||., !||!

on :: Applicative (Color cs) => Pixel cs Bit
on = pure one

off :: Applicative (Color cs) => Pixel cs Bit
off = pure zero

isOn :: Pixel Model.Y Bit -> Bool
isOn = (== on)

isOff :: Pixel Model.Y Bit -> Bool
isOff = (== off)


-- | 'Thresholding' contains a convenient set of functions for binary image
-- construction, which is done by comparing either a single pixel with every
-- pixel in an image or two same size images pointwise. For example:
--
-- >>> frog <- readImageY "images/frog.jpg"
-- >>> frog .==. PixelY 0    -- (or: PixelY 0 .==. frog)
-- >>> frog .<. flipH frog   -- (or: flipH frog .>. frog)
--
class Thresholding a b where
  threshold2 ::
       (ColorModel cs e', ColorModel cs e, ColorModel cs Bit)
    => Pixel cs (e' -> e -> Bool)
    -> a cs e'
    -> b cs e
    -> Image cs Bit
  thresholdWith2 ::
       (ColorModel cs e)
    => (Pixel cs e -> Pixel cs e -> Bool) -- ^ Predicate
    -> a cs e -- ^ First source image.
    -> b cs e -- ^ Second source image.
    -> Image Model.Y Bit



instance Thresholding Image Image where
  threshold2 f = I.zipWith (\ px1 px2 -> fromBool <$> (f <*> px1 <*> px2))
  {-# INLINE threshold2 #-}
  thresholdWith2 f = I.zipWith (\ px1 px2 -> pure $ fromBool $ f px1 px2)
  {-# INLINE thresholdWith2 #-}


instance Thresholding Pixel Image where
  threshold2 f px1 = I.map (\ px2 -> fromBool <$> (f <*> px1 <*> px2))
  {-# INLINE threshold2 #-}
  thresholdWith2 f px1 = I.map (\ px2 -> pure $ fromBool $ f px1 px2)
  {-# INLINE thresholdWith2 #-}


instance Thresholding Image Pixel where
  threshold2 f img px2 = I.map (\ px1 -> fromBool <$> (f <*> px1 <*> px2)) img
  {-# INLINE threshold2 #-}
  thresholdWith2 f img px2 = I.map (\ px1 -> pure $ fromBool $ f px1 px2) img
  {-# INLINE thresholdWith2 #-}


(!==!), (!/=!) ::
     (Thresholding a b, ColorModel cs e, ColorModel cs Bit) => a cs e -> b cs e -> Image cs Bit
(!==!) = threshold2 (pure (==))
{-# INLINE (!==!) #-}
(!/=!) = threshold2 (pure (/=))
{-# INLINE (!/=!) #-}

(!<!), (!<=!), (!>!), (!>=!) ::
     (Thresholding a b, ColorModel cs e, ColorModel cs Bit, Ord e)
  => a cs e
  -> b cs e
  -> Image cs Bit
(!<!)  = threshold2 (pure (<))
{-# INLINE (!<!) #-}
(!<=!) = threshold2 (pure (<=))
{-# INLINE (!<=!) #-}
(!>!)  = threshold2 (pure (>))
{-# INLINE (!>!) #-}
(!>=!) = threshold2 (pure (>=))
{-# INLINE (!>=!) #-}


(.==.), (./=.) :: (Thresholding a b, ColorModel cs e) => a cs e -> b cs e -> Image Model.Y Bit
(.==.) = thresholdWith2 (==)
{-# INLINE (.==.) #-}
(./=.) = thresholdWith2 (/=)
{-# INLINE (./=.) #-}

(.<.), (.<=.), (.>.), (.>=.) ::
     (Thresholding a b, ColorModel cs e, Ord (Color cs e)) => a cs e -> b cs e -> Image Model.Y Bit
(.<.)  = thresholdWith2 (<)
{-# INLINE (.<.) #-}
(.<=.) = thresholdWith2 (<=)
{-# INLINE (.<=.) #-}
(.>.)  = thresholdWith2 (>)
{-# INLINE (.>.) #-}
(.>=.) = thresholdWith2 (>=)
{-# INLINE (.>=.) #-}


-- TODO: validate optimization rewrite rule:
--{-# RULES
--"bit/bool/binary" forall x. fromBool (bit2bool x) = pure x
-- #-}
-- | Pixel wise @AND@ operator on binary images. Unlike `!&&!` this operator
-- will also @AND@ pixel componenets.
(.&&.), (.||.) :: (Thresholding a b, ColorModel cs Bit) =>
                  a cs Bit -> b cs Bit -> Image Model.Y Bit
(.&&.) = thresholdWith2 (\px1 px2 -> toBool $ F.foldl' (.&.) one $ liftA2 (.&.) px1 px2)
{-# INLINE (.&&.) #-}

-- | Pixel wise @OR@ operator on binary images. Unlike `!||!` this operator
-- will also @OR@ pixel componenets.
(.||.) = thresholdWith2 (\px1 px2 -> toBool $ F.foldl' (.|.) zero $ liftA2 (.|.) px1 px2)
{-# INLINE (.||.) #-}


-- TODO: validate optimization rewrite rule:
--{-# RULES
--"bit/bool/bit" forall x. bool2bit (bit2bool x) = x
-- #-}
-- | Pixel wise @AND@ operator on binary images.
(!&&!), (!||!) :: (Thresholding a b, ColorModel cs Bit) =>
          a cs Bit -> b cs Bit -> Image cs Bit
(!&&!) = threshold2 (pure (\e1 e2 -> toBool (e1 .&. e2)))
{-# INLINE (!&&!) #-}

-- | Pixel wise @OR@ operator on binary images.
(!||!) = threshold2 (pure (\e1 e2 -> toBool (e1 .|. e2)))
{-# INLINE (!||!) #-}


-- | Complement each pixel in a binary image
invert :: ColorModel cs Bit => Image cs Bit -> Image cs Bit
invert = I.map (fmap complement)
{-# INLINE invert #-}


-- | Construct a binary image using a predicate from a source image.
thresholdWith ::
     ColorModel cs e
  => (Pixel cs e -> Bool) -- ^ Predicate
  -> Image cs e -- ^ Source image.
  -> Image Model.Y Bit
thresholdWith f = I.map (pure . fromBool . f)
{-# INLINE thresholdWith #-}


-- | Threshold an image by supplying a thresholding function per channel.
threshold :: (ColorModel cs e, ColorModel cs Bit) =>
             Pixel cs (e -> Bool) -> Image cs e -> Image cs Bit
threshold f = I.map (fmap fromBool . (f <*>))
{-# INLINE threshold #-}


-- | Join each component of a pixel with a binary @`.|.`@ operator.
disjunction, conjunction :: (ColorModel cs Bit) => Image cs Bit -> Image Model.Y Bit
disjunction = I.map (pure . F.foldl' (.|.) zero)
{-# INLINE disjunction #-}

-- | Join each component of a pixel with a binary @`.&.`@ operator.
conjunction = I.map (pure . F.foldl' (.&.) one)
{-# INLINE conjunction #-}


-- | Disjunction of all pixels in a Binary image
--
-- >>> or (makeImage (Sz2 1 2) (const 0) :: Image Model.Y Bit)
-- False
--
-- >>> or (makeImage (Sz2 1 2) (\(Ix2 _ j) -> pure (fromNum j)) :: Image Model.Y Bit)
-- True
or :: Image Model.Y Bit -> Bool
or = getAny . I.foldMono (Any . (== pure one))
{-# INLINE or #-}

-- | Conjunction of all pixels in a Binary image
and :: Image Model.Y Bit -> Bool
and =  getAll . I.foldMono (All . (== pure one))
{-# INLINE and #-}


{- $morphology In order to demonstrate how morphological operations work, a
/binary source image/ = __B__ constructed here together with a /structuring element/
= __S__ will be used in examples that follow. Origin of the structuring
element is always at it's center, eg. @(1,1)@ for the one below.

@
figure :: Image Model.Y Bit
figure = fromLists [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,0],
                    [0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                    [0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],
                    [0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0],
                    [0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
                    [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]
struct :: Image Model.Y Bit
struct = fromLists [[0,1,0],[1,1,0],[0,1,0]]
@
-}


-- | Erosion is defined as: __{E = B ⊖ S = {m,n|Sₘₙ⊆B}__
--
-- >>> writeImageExact PNG [] "images/figure_erode.png" $ pixelGrid 10 $ fromImageBinary $ erode struct figure
--
-- <<images/figure.png>> eroded with <<images/struct.png>> is <<images/figure_erode.png>>
--
erode :: ColorModel cs Bit
      => Image cs Bit -- ^ Structuring element.
      -> Image cs Bit -- ^ Binary source image.
      -> Image cs Bit
erode struc = invert . convolve (Fill (pure one)) struc . invert
{-# INLINE erode #-}


-- | Dialation is defined as: __{D = B ⊕ S = {m,n|Sₘₙ∩B≠∅}__
--
-- >>> writeImageExact PNG [] "images/figure_dialate.png" $ pixelGrid 10 $ fromImageBinary $ dialate struct figure
--
-- <<images/figure.png>> dialated with <<images/struct.png>> is <<images/figure_dialate.png>>
--
dialate :: ColorModel cs Bit
        => Image cs Bit -- ^ Structuring element.
        -> Image cs Bit -- ^ Binary source image.
        -> Image cs Bit
dialate = convolve (Fill (pure zero))
{-# INLINE dialate #-}


-- | Opening is defined as: __{B ○ S = (B ⊖ S) ⊕ S}__
--
-- >>> writeImageExact PNG [] "images/figure_open.png" $ pixelGrid 10 $ fromImageBinary $ open struct figure
--
-- <<images/figure.png>> opened with <<images/struct.png>> is <<images/figure_open.png>>
--
open :: ColorModel cs Bit
     => Image cs Bit -- ^ Structuring element.
     -> Image cs Bit -- ^ Binary source image.
     -> Image cs Bit
open struc = dialate struc . erode struc
{-# INLINE open #-}


-- | Closing is defined as: __{B ● S = (B ⊕ S) ⊖ S}__
--
-- >>> writeImageExact PNG [] "images/figure_close.png" $ pixelGrid 10 $ fromImageBinary $ close struct figure
--
-- <<images/figure.png>> closed with <<images/struct.png>> is <<images/figure_close.png>>
--
close :: ColorModel cs Bit
      => Image cs Bit -- ^ Structuring element.
      -> Image cs Bit -- ^ Binary source image.
      -> Image cs Bit
close struc = erode struc . dialate struc
{-# INLINE close #-}

