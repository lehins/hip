{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, ViewPatterns, BangPatterns, TypeFamilies, UndecidableInstances #-}

module Graphics.Image.Interface (
  sum, minimum, maximum, normalize,
  Convertable(..),
  Pixel(..),
  Strategy(..),
  Image(..),
  Interpolation(..)
  ) where

import Prelude hiding ((++), map, sum, minimum, maximum)

class Convertable a b where
  convert :: a -> b


class (Num px, Show px, Ord (Inner px), Num (Inner px)) => Pixel px where
  type Inner px :: *
  
  pixel :: Inner px -> px
       
  pxOp :: (Inner px -> Inner px) -> px -> px

  pxOp2 :: (Inner px -> Inner px -> Inner px) -> px -> px -> px

  strongest :: px -> px

  weakest :: px -> px

  showType :: px -> String


class (Image img px, Pixel px) => Strategy strat img px where
  
  -- | Make sure an Image is in a computed form.
  compute :: Pixel px =>
             strat img px -- ^ a strategy for computing this image.
             -> img px    -- ^ image to be computed.
             -> img px

  -- | Fold an Image.
  fold :: Pixel px =>
          strat img px
          -> (px -> px -> px)
          -> px
          -> img px
          -> px

  -- | Convert an Image to a list of lists of Pixels.
  toLists :: Pixel px =>
             strat img px
             -> img px
             -> [[px]]
  toLists strat img =
    [[index img' m n | n <- [0..cols img - 1]] | m <- [0..rows img - 1]] where
      img' = compute strat img


class (Num (img px), Show (img px), Pixel px) => Image img px | px -> img where

  -- | Get dimensions of the image. (rows, cols)
  dims :: img px -> (Int, Int)

  -- | Get the number of rows in the image 
  rows :: img px -> Int
  rows = fst . dims

  -- | Get the number of columns in the image
  cols :: Pixel px => img px -> Int
  cols = snd . dims

  -- | Get a pixel at i-th row and j-th column
  index :: img px
        -> Int -> Int
        -> px

  -- | Get a pixel at i-th row and j-th column without bounds check.
  unsafeIndex :: img px
              -> Int -> Int
              -> px
  -- | Make an Image by supplying number of rows, columns and a function that
  -- returns a pixel value at the m n location which are provided as arguments.
  make :: Int -> Int -> (Int -> Int -> px) -> img px

  {-| Map a function over an image. -}
  map :: Pixel px1 => (px1 -> px) -> img px1 -> img px

  {-| Apply a function to every pixel of an image and its index. -}
  imap :: Pixel px1 => (Int -> Int -> px1 -> px) -> img px1 -> img px
  imap !op !img = traverse img (,) getNewPx where
    getNewPx !getPx !i !j = op i j (getPx i j)
    {-# INLINE getNewPx #-}
  {-# INLINE imap #-}
  
  -- | Zip two Images with a function. Images do not have to hold the same type
  -- of pixels.
  zipWith :: (Pixel px1, Pixel px2) =>
             (px1 -> px2 -> px)
          -> img px1
          -> img px2
          -> img px

  -- | Traverse an image.
  traverse :: Pixel px1 =>
              img px1
           -> (Int -> Int -> (Int, Int))
           -> ((Int -> Int -> px1) -> Int -> Int -> px)
           -> img px

  -- | Traverse two images.
  traverse2 :: (Pixel px1, Pixel px2) =>
               img px1
            -> img px2
            -> (Int -> Int -> Int -> Int -> (Int, Int))
            -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> Int -> Int -> px)
            -> img px

  -- | Traverse three images.
  traverse3 :: (Pixel px1, Pixel px2, Pixel px3) =>
               img px1
            -> img px2
            -> img px3
            -> (Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int))
            -> ((Int -> Int -> px1) ->
                (Int -> Int -> px2) ->
                (Int -> Int -> px3) ->
                Int -> Int -> px)
            -> img px

  -- | Transpose an image
  transpose :: img px
            -> img px

  -- | Backpermute an Image
  backpermute :: Int -> Int -- ^ rows and columns in a new image.
              -> (Int -> Int -> (Int, Int)) -- ^ Function that maps each
                                            -- location in a new image to an old
                                            -- image
              -> img px -- ^ source image
              -> img px

  -- | Crop an image retrieves a sub-image from a source image with @m@ rows and
  -- @n@ columns. Make sure @(m + i, n + j)@ is not greater than dimensions of a
  -- source image.
  crop :: Int -> Int -- ^ Starting index @i@ @j@ from within an old image
       -> Int -> Int -- ^ Dimensions of a new image @m@ and @n@.
       -> img px   -- ^ Source image.
       -> img px

  -- | Convert a nested List of Pixels to an Image.
  fromLists :: [[px]] -> img px


class Interpolation alg where
  interpolate :: (RealFrac (Inner px), Pixel px) =>
                 alg                -- ^ Interpolation algorithm
              -> px                 -- ^ Default pixel to be used when out of bound.
              -> Int -> Int         -- ^ Image dimensions @m@ rows and @n@ columns.
              -> (Int -> Int -> px) -- ^ Lookup function that returns a pixel at @i@th
                                    -- and @j@th location.
              -> (Inner px) -> (Inner px)   -- ^ real values of @i@ and @j@ index
              -> px


-- | Sum all pixels of the image
sum :: (Strategy strat img px, Image img px, Pixel px) =>
       strat img px
       -> img px
       -> px
sum strat img = fold strat (+) (index img 0 0) img
{-# INLINE sum #-}

maximum :: (Strategy strat img px, Image img px, Pixel px, Ord px) =>
           strat img px
           -> img px
           -> px
maximum strat img = fold strat (pxOp2 max) (index img 0 0) img
{-# INLINE maximum #-}

minimum :: (Strategy strat img px, Image img px, Pixel px, Ord px) =>
           strat img px
           -> img px
           -> px
minimum strat img = fold strat (pxOp2 min) (index img 0 0) img
{-# INLINE minimum #-}
  
normalize :: (Strategy strat img px, Image img px, Pixel px, Fractional px, Ord px) =>
             strat img px
             -> img px
             -> img px
normalize strat img = compute strat $ if s == w
                then (if s < 0 then (map (*0)) else
                        if s > 1 then (map (*1)) else id) img
                else map normalizer img where
                  !(s, w) = (strongest $ maximum strat img,
                             weakest $ minimum strat img)
                  normalizer px = (px - w)/(s - w)
                  {-# INLINE normalizer #-}
{-# INLINE normalize #-}
