{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module      : Graphics.Image.Processing.Binary
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Binary (
  -- * Construction
  toImageBinaryUsing, toImageBinaryUsing2,
  threshold, threshold2,
  thresholdWith, thresholdWith2, compareWith,
  -- * Thresholding
  Thresholding(..),
  -- * Bitwise operations
  or, and, (!&&!), (!||!), (.&&.), (.||.), invert, disjunction, conjunction,
  -- * Binary Morphology
  -- $morphology
  erode, dialate, open, close
  ) where

import           Control.Applicative
import           Data.Bits
import qualified Data.Foldable                           as F
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface                as I
import           Graphics.Image.Interface.Vector.Unboxed (VU)
import           Graphics.Image.Internal                 as I
import           Graphics.Image.Processing.Convolution
import           Graphics.Image.Utils                    ((.:), (.:!))
import           Prelude                                 as P hiding (and, or)

infix  4  .==., ./=., .<., .<=., .>=., .>., !==!, !/=!, !<!, !<=!, !>=!, !>!
infixr 3  .&&., !&&!
infixr 2  .||., !||!



-- | 'Thresholding' contains a convenient set of functions for binary image
-- construction, which is done by comparing either a single pixel with every
-- pixel in an image or two same size images pointwise. For example:
--
-- >>> frog <- readImageY VU "images/frog.jpg"
-- >>> frog .==. PixelY 0    -- (or: PixelY 0 .==. frog)
-- >>> frog .<. flipH frog   -- (or: flipH frog .>. frog)
--
class Thresholding a b arr | a b -> arr where
  (!==!) :: (Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
            a cs e -> b cs e -> Image arr cs Bit
  (!/=!) :: (Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
            a cs e -> b cs e -> Image arr cs Bit
  (!<!)  :: (Ord e, Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
            a cs e -> b cs e -> Image arr cs Bit
  (!<=!) :: (Ord e, Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
            a cs e -> b cs e -> Image arr cs Bit
  (!>!)  :: (Ord e, Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
            a cs e -> b cs e -> Image arr cs Bit
  (!>=!) :: (Ord e, Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
            a cs e -> b cs e -> Image arr cs Bit
  (.==.) :: (Array arr cs e, Array arr X Bit) => a cs e -> b cs e -> Image arr X Bit
  (./=.) :: (Array arr cs e, Array arr X Bit) => a cs e -> b cs e -> Image arr X Bit
  (.<.)  :: (Ord (Pixel cs e), Array arr cs e, Array arr X Bit) =>
            a cs e -> b cs e -> Image arr X Bit
  (.<=.) :: (Ord (Pixel cs e), Array arr cs e, Array arr X Bit) =>
            a cs e -> b cs e -> Image arr X Bit
  (.>.)  :: (Ord (Pixel cs e), Array arr cs e, Array arr X Bit) =>
            a cs e -> b cs e -> Image arr X Bit
  (.>=.) :: (Ord (Pixel cs e), Array arr cs e, Array arr X Bit) =>
            a cs e -> b cs e -> Image arr X Bit



instance Thresholding (Image arr) (Image arr) arr where
  (.==.) = toImageBinaryUsing2 (==)
  {-# INLINE (.==.) #-}
  (./=.) = toImageBinaryUsing2 (/=)
  {-# INLINE (./=.) #-}
  (.<.)  = toImageBinaryUsing2 (<)
  {-# INLINE (.<.) #-}
  (.<=.) = toImageBinaryUsing2 (<=)
  {-# INLINE (.<=.) #-}
  (.>.)  = toImageBinaryUsing2 (>)
  {-# INLINE (.>.) #-}
  (.>=.) = toImageBinaryUsing2 (>=)
  {-# INLINE (.>=.) #-}
  (!==!) = threshold2 (pure (==))
  {-# INLINE (!==!) #-}
  (!/=!) = threshold2 (pure (/=))
  {-# INLINE (!/=!) #-}
  (!<!)  = threshold2 (pure (<))
  {-# INLINE (!<!) #-}
  (!<=!) = threshold2 (pure (<=))
  {-# INLINE (!<=!) #-}
  (!>!)  = threshold2 (pure (>))
  {-# INLINE (!>!) #-}
  (!>=!) = threshold2 (pure (>=))
  {-# INLINE (!>=!) #-}


instance Array arr X Bit => Thresholding Pixel (Image arr) arr where
  (.==.) !px = toImageBinaryUsing (==px)
  {-# INLINE (.==.) #-}
  (./=.) !px = toImageBinaryUsing (/=px)
  {-# INLINE (./=.) #-}
  (.<.)  !px = toImageBinaryUsing (< px)
  {-# INLINE (.<.) #-}
  (.<=.) !px = toImageBinaryUsing (<=px)
  {-# INLINE (.<=.) #-}
  (.>.)  !px = toImageBinaryUsing (> px)
  {-# INLINE (.>.) #-}
  (.>=.) !px = toImageBinaryUsing (>=px)
  {-# INLINE (.>=.) #-}
  (!==!) !px = threshold ((==) <$> px)
  {-# INLINE (!==!) #-}
  (!/=!) !px = threshold ((/=) <$> px)
  {-# INLINE (!/=!) #-}
  (!<!) !px  = threshold ((<) <$> px)
  {-# INLINE (!<!) #-}
  (!<=!) !px = threshold ((<=) <$> px)
  {-# INLINE (!<=!) #-}
  (!>!) !px  = threshold ((>) <$> px)
  {-# INLINE (!>!) #-}
  (!>=!) !px = threshold ((>=) <$> px)
  {-# INLINE (!>=!) #-}


instance Array arr X Bit => Thresholding (Image arr) Pixel arr where
  (.==.) !img !px = toImageBinaryUsing (==px) img
  {-# INLINE (.==.) #-}
  (./=.) !img !px = toImageBinaryUsing (/=px) img
  {-# INLINE (./=.) #-}
  (.<.)  !img !px = toImageBinaryUsing (< px) img
  {-# INLINE (.<.) #-}
  (.<=.) !img !px = toImageBinaryUsing (<=px) img
  {-# INLINE (.<=.) #-}
  (.>.)  !img !px = toImageBinaryUsing (> px) img
  {-# INLINE (.>.) #-}
  (.>=.) !img !px = toImageBinaryUsing (>=px) img
  {-# INLINE (.>=.) #-}
  (!==!) !img !px = threshold ((==) <$> px) img
  {-# INLINE (!==!) #-}
  (!/=!) !img !px = threshold ((/=) <$> px) img
  {-# INLINE (!/=!) #-}
  (!<!)  !img !px = threshold ((<) <$> px) img
  {-# INLINE (!<!) #-}
  (!<=!) !img !px = threshold ((<=) <$> px) img
  {-# INLINE (!<=!) #-}
  (!>!)  !img !px = threshold ((>) <$> px) img
  {-# INLINE (!>!) #-}
  (!>=!) !img !px = threshold ((>=) <$> px) img
  {-# INLINE (!>=!) #-}


-- | Pixel wise @AND@ operator on binary images. Unlike `!&&!` this operator
-- will also @AND@ pixel componenets.
(.&&.) :: (Array arr cs Bit, Array arr X Bit) =>
          Image arr cs Bit -> Image arr cs Bit -> Image arr X Bit
(.&&.) = squashWith2 ((.&.) .: (.&.)) one
{-# INLINE (.&&.) #-}

-- | Pixel wise @OR@ operator on binary images. Unlike `!||!` this operator
-- will also @OR@ pixel componenets.
(.||.) :: (Array arr cs Bit, Array arr X Bit) =>
          Image arr cs Bit -> Image arr cs Bit -> Image arr X Bit
(.||.) = squashWith2 ((.|.) .: (.|.)) zero
{-# INLINE (.||.) #-}


-- | Pixel wise @AND@ operator on binary images.
(!&&!) :: Array arr cs Bit =>
          Image arr cs Bit -> Image arr cs Bit -> Image arr cs Bit
(!&&!) = I.zipWith (liftPx2 (.&.))
{-# INLINE (!&&!) #-}

-- | Pixel wise @OR@ operator on binary images.
(!||!) :: Array arr cs Bit =>
          Image arr cs Bit -> Image arr cs Bit -> Image arr cs Bit
(!||!) = I.zipWith (liftPx2 (.|.))
{-# INLINE (!||!) #-}


-- | Complement each pixel in a binary image
invert :: Array arr cs Bit => Image arr cs Bit -> Image arr cs Bit
invert = I.map (liftPx complement)
{-# INLINE invert #-}


-- | Construct a binary image using a predicate from a source image.
toImageBinaryUsing :: (Array arr cs e, Array arr X Bit) =>
                      (Pixel cs e -> Bool) -- ^ Predicate
                   -> Image arr cs e -- ^ Source image.
                   -> Image arr X Bit
toImageBinaryUsing f = I.map (fromBool . f)
{-# INLINE toImageBinaryUsing #-}


-- | Construct a binary image using a predicate from two source images.
toImageBinaryUsing2 :: (Array arr cs e, Array arr X Bit) =>
                       (Pixel cs e -> Pixel cs e -> Bool) -- ^ Predicate
                    -> Image arr cs e -- ^ First source image.
                    -> Image arr cs e -- ^ Second source image.
                    -> Image arr X Bit
toImageBinaryUsing2 f = I.zipWith (fromBool .:! f)
{-# INLINE toImageBinaryUsing2 #-}


threshold :: (Applicative (Pixel cs), Array arr cs e, Array arr cs Bit) =>
             Pixel cs (e -> Bool) -> Image arr cs e -> Image arr cs Bit
threshold fPx = I.map (fmap bool2bit . (fPx <*>))
{-# INLINE threshold #-}


threshold2 :: (Applicative (Pixel cs), Array arr cs e', Array arr cs e, Array arr cs Bit) =>
              Pixel cs (e' -> e -> Bool)
           -> Image arr cs e'
           -> Image arr cs e
           -> Image arr cs Bit
threshold2 fPx = I.zipWith (\ !px1 !px2 -> bool2bit <$> (fPx <*> px1 <*> px2))
{-# INLINE threshold2 #-}


-- | Threshold a source image with an applicative pixel.
--
-- >>> yield <- readImageRGB VU "images/yield.jpg"
-- >>> writeImageExact PNG [] "images/yield_bin.png" $ thresholdWith (PixelRGB (>0.55) (<0.6) (<0.5)) yield
--
-- <<images/yield.jpg>> <<images/yield_bin.png>>
--
thresholdWith :: (Applicative (Pixel cs), Foldable (Pixel cs),
                  Array arr cs e, Array arr X Bit) =>
                 Pixel cs (e -> Bool)
                 -- ^ Pixel containing a thresholding function per channel.
              -> Image arr cs e -- ^ Source image.
              -> Image arr X Bit
thresholdWith fPx = I.map (fromBool . F.and . (fPx <*>))
{-# INLINE thresholdWith #-}


-- | Compare two images with an applicative pixel. Works just like
-- 'thresholdWith', but on two images.
thresholdWith2 :: (Applicative (Pixel cs), Foldable (Pixel cs),
                   Array arr cs e1, Array arr cs e2, Array arr X Bit) =>
                  Pixel cs (e1 -> e2 -> Bool)
                  -- ^ Pixel containing a comparing function per channel.
               -> Image arr cs e1 -- ^ First image.
               -> Image arr cs e2 -- ^ second image.
               -> Image arr X Bit
thresholdWith2 fPx = I.zipWith (\ !px1 !px2 -> (fromBool . F.and) (fPx <*> px1 <*> px2))
{-# INLINE thresholdWith2 #-}
-- I.map (\ !px -> PixelX $ foldlPx (.&.) one $ (fmap (bool2bit .) fPx <*> px))

-- | Compare two images with an applicative pixel. Works just like
-- 'thresholdWith', but on two images.
compareWith :: (Applicative (Pixel cs), Foldable (Pixel cs),
                Array arr cs e1, Array arr cs e2, Array arr X Bit) =>
               Pixel cs (e1 -> e2 -> Bool)
               -- ^ Pixel containing a comparing function per channel.
            -> Image arr cs e1 -- ^ First image.
            -> Image arr cs e2 -- ^ second image.
            -> Image arr X Bit
compareWith = thresholdWith2
{-# INLINE compareWith #-}
{-# DEPRECATED compareWith "Use `thresholdWith2` instead." #-}


-- | Join each component of a pixel with a binary @`.|.`@ operator.
disjunction :: (Array arr cs Bit, Array arr X Bit) =>
               Image arr cs Bit -> Image arr X Bit
disjunction = squashWith (.|.) zero
{-# INLINE disjunction #-}


-- | Join each component of a pixel with a binary @`.&.`@ operator.
conjunction :: (Array arr cs Bit, Array arr X Bit) =>
               Image arr cs Bit -> Image arr X Bit
conjunction = squashWith (.&.) one
{-# INLINE conjunction #-}

-- | Disjunction of all pixels in a Binary image
or :: Array arr X Bit => Image arr X Bit -> Bool
or = isOn . fold (.|.) off
{-# INLINE or #-}


-- | Conjunction of all pixels in a Binary image
and :: Array arr X Bit => Image arr X Bit -> Bool
and = isOn . fold (.&.) on
{-# INLINE and #-}


{- $morphology In order to demonstrate how morphological operations work, a
/binary source image/ = __B__ constructed here together with a /structuring element/
= __S__ will be used in examples that follow. Origin of the structuring
element is always at it's center, eg. @(1,1)@ for the one below.

@
figure :: Image VU X Bit
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
struct :: Image VU X Bit
struct = fromLists [[0,1,0],[1,1,0],[0,1,0]]
@
-}


-- | Erosion is defined as: __{E = B ⊖ S = {m,n|Sₘₙ⊆B}__
--
-- >>> writeImageExact PNG [] "images/figure_erode.png" $ pixelGrid 10 $ fromImageBinary $ erode struct figure
--
-- <<images/figure.png>> eroded with <<images/struct.png>> is <<images/figure_erode.png>>
--
erode :: (Array VU X Bit, Array arr X Bit) =>
         Image VU X Bit -- ^ Structuring element.
      -> Image arr X Bit -- ^ Binary source image.
      -> Image arr X Bit
erode !struc !img = invert $ convolve (Fill on) struc (invert img)
{-# INLINE erode #-}


-- | Dialation is defined as: __{D = B ⊕ S = {m,n|Sₘₙ∩B≠∅}__
--
-- >>> writeImageExact PNG [] "images/figure_dialate.png" $ pixelGrid 10 $ fromImageBinary $ dialate struct figure
--
-- <<images/figure.png>> dialated with <<images/struct.png>> is <<images/figure_dialate.png>>
--
dialate :: (Array VU X Bit, Array arr X Bit) =>
           Image VU X Bit -- ^ Structuring element.
        -> Image arr X Bit -- ^ Binary source image.
        -> Image arr X Bit
dialate !struc !img = convolve (Fill off) struc img
{-# INLINE dialate #-}


-- | Opening is defined as: __{B ○ S = (B ⊖ S) ⊕ S}__
--
-- >>> writeImageExact PNG [] "images/figure_open.png" $ pixelGrid 10 $ fromImageBinary $ open struct figure
--
-- <<images/figure.png>> opened with <<images/struct.png>> is <<images/figure_open.png>>
--
open :: (Array VU X Bit, Array arr X Bit) =>
        Image VU X Bit -- ^ Structuring element.
     -> Image arr X Bit -- ^ Binary source image.
     -> Image arr X Bit
open !struc = dialate struc . erode struc
{-# INLINE open #-}


-- | Closing is defined as: __{B ● S = (B ⊕ S) ⊖ S}__
--
-- >>> writeImageExact PNG [] "images/figure_close.png" $ pixelGrid 10 $ fromImageBinary $ close struct figure
--
-- <<images/figure.png>> closed with <<images/struct.png>> is <<images/figure_close.png>>
--
close :: (Array VU X Bit, Array arr X Bit) =>
         Image VU X Bit -- ^ Structuring element.
      -> Image arr X Bit -- ^ Binary source image.
      -> Image arr X Bit
close !struc = erode struc . dialate struc
{-# INLINE close #-}

