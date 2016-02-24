{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses #-}
module Graphics.Image.Processing.Binary (
  -- * Binary
  -- ** Thresholding
  Thresholding(..),
  toImageBinaryUsing, toImageBinaryUsing2,
  -- ** Operators
  (.&&.), (.||.), invert,
  -- ** Morphology
  -- $morphology
  erode, dialate, open, close
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface hiding (makeImage)
import Graphics.Image.ColorSpace.Binary
import Graphics.Image.Processing.Convolution


infix  4  .==., ./=., .<., .<=., .>=., .>.
infixr 3  .&&.
infixr 2  .||.


-- | 'Thresholding' contains a convenient set of functions for binary image
-- construction, which is done by comparing either a single pixel with every
-- pixel in an image or two same size images pointwise.
class Array arr Binary Bit => Thresholding a b arr | a b -> arr where
  (.==.) :: (Eq (Pixel cs e), Array arr cs e)  => a cs e -> b cs e -> Image arr Binary Bit
  (./=.) :: (Eq (Pixel cs e), Array arr cs e)  => a cs e -> b cs e -> Image arr Binary Bit
  (.<.)  :: (Ord (Pixel cs e), Array arr cs e) => a cs e -> b cs e -> Image arr Binary Bit
  (.<=.) :: (Ord (Pixel cs e), Array arr cs e) => a cs e -> b cs e -> Image arr Binary Bit
  (.>.)  :: (Ord (Pixel cs e), Array arr cs e) => a cs e -> b cs e -> Image arr Binary Bit
  (.>=.) :: (Ord (Pixel cs e), Array arr cs e) => a cs e -> b cs e -> Image arr Binary Bit



instance Array arr Binary Bit => Thresholding (Image arr) (Image arr) arr where
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
  

instance Array arr Binary Bit => Thresholding Pixel (Image arr) arr where
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
  

instance Array arr Binary Bit => Thresholding (Image arr) Pixel arr where
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


-- | Pixel wise @AND@ operator on binary images. 
(.&&.) :: Array arr Binary Bit =>
          Image arr Binary Bit -> Image arr Binary Bit -> Image arr Binary Bit
(.&&.) = zipWith (*)
{-# INLINE (.&&.) #-}

-- | Pixel wise @OR@ operator on binary images.
(.||.) :: Array arr Binary Bit =>
          Image arr Binary Bit -> Image arr Binary Bit -> Image arr Binary Bit
(.||.) = zipWith (+)
{-# INLINE (.||.) #-}


-- | Complement each pixel in the image
invert :: Array arr Binary Bit => Image arr Binary Bit -> Image arr Binary Bit
invert = map complement
{-# INLINE invert #-}


-- | Construct a binary image using a predicate from a source image.
toImageBinaryUsing :: (Array arr cs e, Array arr Binary Bit) =>
                      (Pixel cs e -> Bool) -- ^ Predicate
                   -> Image arr cs e -- ^ Source image.
                   -> Image arr Binary Bit
toImageBinaryUsing !f = map (fromBool . f)
{-# INLINE toImageBinaryUsing #-}


-- | Construct a binary image using a predicate from two source images.
toImageBinaryUsing2 :: (Array arr cs e, Array arr Binary Bit) =>
                       (Pixel cs e -> Pixel cs e -> Bool) -- ^ Predicate
                    -> Image arr cs e -- ^ First source image.
                    -> Image arr cs e -- ^ Second source image.
                    -> Image arr Binary Bit
toImageBinaryUsing2 !f =  zipWith (((.).(.)) fromBool f)
{-# INLINE toImageBinaryUsing2 #-}


{- $morphology In order to demonstrate how morphological operations work, a
/binary source image/ = __B__ constructed here together with a /structuring element/ =
__S__ will be used in examples that follow.

@
figure :: Image VU Binary Bit
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
struct :: Image VU Binary Bit
struct = fromLists [[0,1],[1,1],[0,1]]
@
-}


-- | Erosion is defined as: __{E = B ⊖ S = {m,n|Sₘₙ⊆B}__
--
-- >>> writeImageExact PNG [] "images/figure_erode.png" $ pixelGrid 10 $ fromImageBin $ erode struct figure
--
-- <<images/figure.png>> eroded with <<images/struct.png>> is <<images/figure_erode.png>>
--
erode :: (Thresholding (Image arr) Pixel arr) =>
         Image arr Binary Bit -- ^ Structuring element.
      -> Image arr Binary Bit -- ^ Binary source image.
      -> Image arr Binary Bit
erode !struc !img = invert (convolve' (Fill on) struc (invert img) ./=. off)
{-# INLINE erode #-}


-- | Dialation is defined as: __{D = B ⊕ S = {m,n|Sₘₙ∩B≠∅}__
--
-- >>> writeImageExact PNG [] "images/figure_dialate.png" $ pixelGrid 10 $ fromImageBin $ dialate struct figure
--
-- <<images/figure.png>> dialated with <<images/struct.png>> is <<images/figure_dialate.png>>
--
dialate :: (Thresholding (Image arr) Pixel arr) =>
           Image arr Binary Bit -- ^ Structuring element.
        -> Image arr Binary Bit -- ^ Binary source image.
        -> Image arr Binary Bit
dialate !struc !img = convolve' (Fill off) struc img ./=. off
{-# INLINE dialate #-}


-- | Opening is defined as: __{B ○ S = (B ⊖ S) ⊕ S}__
--
-- >>> writeImageExact PNG [] "images/figure_open.png" $ pixelGrid 10 $ fromImageBin $ open struct figure
--
-- <<images/figure.png>> opened with <<images/struct.png>> is <<images/figure_open.png>>
--
open :: (Thresholding (Image arr) Pixel arr) =>
        Image arr Binary Bit -- ^ Structuring element.
     -> Image arr Binary Bit -- ^ Binary source image.
     -> Image arr Binary Bit
open struc = dialate struc . erode struc
{-# INLINE open #-}


-- | Closing is defined as: __{B ● S = (B ⊕ S) ⊖ S}__
--
-- >>> writeImageExact PNG [] "images/figure_close.png" $ pixelGrid 10 $ fromImageBin $ close struct figure
--
-- <<images/figure.png>> closed with <<images/struct.png>> is <<images/figure_close.png>>
--
close :: (Thresholding (Image arr) Pixel arr) =>
         Image arr Binary Bit -- ^ Structuring element.
      -> Image arr Binary Bit -- ^ Binary source image.
      -> Image arr Binary Bit
close struc = erode struc . dialate struc
{-# INLINE close #-}


