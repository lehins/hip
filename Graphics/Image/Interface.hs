{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, ViewPatterns, BangPatterns, TypeFamilies, UndecidableInstances #-}

module Graphics.Image.Interface (
  minimum, maximum, normalize,
  Convertable(..),
  Pixel(..),
  Strategy(..),
  Image(..),
  Interpolation(..)
  ) where

import Prelude hiding (map, sum, minimum, maximum)

class Convertable a b where
  convert :: a -> b


class (Eq px, Num px, Show px,
       Eq (Inner px), Num (Inner px), Show (Inner px), Ord (Inner px)) =>
      Pixel px where
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
  {-# MINIMAL (compute, fold) #-}
  
  -- | Sum all pixels of the image
  sum :: (Strategy strat img px, Image img px, Pixel px) =>
         strat img px
         -> img px
         -> px
  sum strat img = fold strat (+) 0 img
  {-# INLINE sum #-}

  -- | Convert an Image to a list of lists of Pixels.
  toLists :: Pixel px =>
             strat img px
             -> img px
             -> [[px]]
  toLists !strat !img =
    [[index img' i j | j <- [0..n - 1]] | i <- [0..m - 1]] where
      !(m, n) = dims img
      !img' = compute strat img
      

{- | This is a core Image interface. -}
class (Num (img px), Show (img px), Pixel px) => Image img px | px -> img where

  -- | Make an Image by supplying number of rows, columns and a function that
  -- returns a pixel value at the m n location which are provided as arguments.
  make :: Int -> Int -> (Int -> Int -> px) -> img px
  
  -- | Get dimensions of the image. (rows, cols)
  dims :: img px -> (Int, Int)

  -- | Get a pixel at i-th row and j-th column without bounds check.
  unsafeIndex :: img px
              -> Int -> Int
              -> px
              
  {-# MINIMAL (make, dims, unsafeIndex) #-}
  
  -- | Get the number of rows in the image 
  rows :: img px -> Int
  rows = fst . dims
  {-# INLINE rows #-}

  -- | Get the number of columns in the image
  cols :: Pixel px => img px -> Int
  cols = snd . dims
  {-# INLINE cols #-}

  -- | Get a pixel at @i@ and @j@ location.
  index :: img px -- ^ Source image.
        -> Int    -- ^ @i@th row
        -> Int    -- ^ @j@th column.
        -> px
  index !img@(dims -> (m, n)) !i !j =
    if i >= 0 && j >= 0 && i < m && j < n then unsafeIndex img i j
    else error ("Index out of bounds for image: "++ show img++". Supplied i="++
                show i++" and j="++show j)
  {-# INLINE index #-}

  -- | Get a pixel at @i@ @j@ location with a default pixel. If index is out of
  -- bounds, default pixel will be returned
  defaultIndex :: px     -- ^ Default pixel.
               -> img px -- ^ Source image.
               -> Int    -- ^ @i@th row
               -> Int    -- ^ @j@th column.
               -> px
  defaultIndex defPx img@(dims -> (m, n)) i j =
    if i >= 0 && j >= 0 && i < m && j < n then unsafeIndex img i j else defPx
  {-# INLINE defaultIndex #-}
  
  -- | Get a pixel at @i@ @j@ location with a default pixel. If index is out of
  -- bounds 'Nothing' is returned, @'Just' px@ otherwise.
  maybeIndex :: img px -- ^ Source image.
             -> Int    -- ^ @i@th row
             -> Int    -- ^ @j@th column.
             -> Maybe px
  maybeIndex img@(dims -> (m, n)) i j =
    if i >= 0 && j >= 0 && i < m && j < n then Just $ unsafeIndex img i j else Nothing
  {-# INLINE maybeIndex #-}
        
  {-| Map a function over an image. -}
  map :: (Image img px1, Pixel px1) => (px1 -> px) -> img px1 -> img px
  map !op !img@(dims -> (m, n)) = make m n getNewPx where
    getNewPx !i !j = op $ index img i j
    {-# INLINE getNewPx #-}
  {-# INLINE map #-}

  {-| Apply a function to every pixel of an image and its index. -}
  imap :: (Image img px1, Pixel px1) => (Int -> Int -> px1 -> px) -> img px1 -> img px
  imap !getPx !img@(dims -> (m, n)) = make m n getNewPx where
    getNewPx !i !j = getPx i j (index img i j)
    {-# INLINE getNewPx #-}
  {-# INLINE imap #-}
  
  -- | Zip two Images with a function.
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

  -- | Crop an image, i.e. retrieves a sub-image image with @m@ rows and @n@
  -- columns. Make sure @(m + i, n + j)@ is not greater than dimensions of a
  -- source image.
  crop :: Int     -- ^ @i@ and 
       -> Int     -- ^ @j@ starting index from within an old image.
       -> Int     -- ^ @m@ rows and
       -> Int     -- ^ @n@ columns. Dimensions of a new image @m@ and @n@.
       -> img px  -- ^ Source image.
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
