{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, ViewPatterns, BangPatterns, TypeFamilies, UndecidableInstances #-}

module HIP.Interface (
  minimum, maximum, normalize,
  Strategy(..),
  AImage(..)
  ) where

import Prelude hiding (map, sum, minimum, maximum)
import qualified Data.Vector as V (Vector)
import HIP.Pixel.Base (Pixel(..))


class AImage img px => Strategy strat img px where
  
  -- | Make sure an AImage is in a computed form.
  compute :: strat img px -- ^ a strategy for computing this image.
             -> img px    -- ^ image to be computed.
             -> img px

  -- | Fold an AImage.
  fold :: strat img px
          -> (px -> px -> px)
          -> px
          -> img px
          -> px

  -- | Convert an image to a flat Boxed Vector
  toBoxedVector :: strat img px -> img px -> V.Vector px
  
  {-# MINIMAL (compute, fold, toBoxedVector) #-}
  
  -- | Sum all pixels of the image
  sum :: (Strategy strat img px, AImage img px, Pixel px) =>
         strat img px
         -> img px
         -> px
  sum strat img = fold strat (+) 0 img
  {-# INLINE sum #-}

  -- | Convert an AImage to a list of lists of Pixels.
  toList :: Pixel px =>
            strat img px
            -> img px
            -> [[px]]
  toList !strat !img =
    [[index img' i j | j <- [0..n - 1]] | i <- [0..m - 1]] where
      !(m, n) = dims img
      !img' = compute strat img

      

{- | This is an abstract image interface. -}
class (Num (img px), Show (img px), Pixel px) => AImage img px | px -> img where

  -- | Make an AImage by supplying number of rows, columns and a function that
  -- returns a pixel value at the m n location which are provided as arguments.
  make :: Int -> Int -> (Int -> Int -> px) -> img px
  
  -- | Get dimensions of the image. (rows, cols)
  dims :: img px -> (Int, Int)

  -- | Get a pixel at i-th row and j-th column without bounds check.
  unsafeIndex :: img px
              -> Int -> Int
              -> px
              
  -- | Zip two AImages with a function.
  zipWith :: (AImage img px1, AImage img px2) =>
             (px1 -> px2 -> px)
          -> img px1
          -> img px2
          -> img px
          
  -- | Convert a nested List of Pixels to an AImage.
  fromList :: [[px]] -> img px
  
  {-# MINIMAL (make, dims, unsafeIndex, zipWith, fromList, fromBoxedVector, (|*|)) #-}
  
  -- | Get the number of rows in the image 
  rows :: img px -> Int
  rows = fst . dims
  {-# INLINE rows #-}

  -- | Get the number of columns in the image
  cols :: img px -> Int
  cols = snd . dims
  {-# INLINE cols #-}

  -- | Get a pixel at @i@ and @j@ location.
  index :: img px -- ^ Source image.
        -> Int    -- ^ @i@th row
        -> Int    -- ^ @j@th column.
        -> px
  index !img@(dims -> (m, n)) !i !j =
    if i >= 0 && j >= 0 && i < m && j < n then unsafeIndex img i j
    else error ("Index out of bounds for image: "++ show img++". Supplied i:"++
                show i++" and j:"++show j)
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
        
  -- | Map a function over an image.
  map :: (AImage img px1) => (px1 -> px) -> img px1 -> img px
  map !op !img@(dims -> (m, n)) = make m n getNewPx where
    getNewPx !i !j = op $ index img i j
    {-# INLINE getNewPx #-}
  {-# INLINE map #-}

  -- | Apply a function to every pixel of an image and its index. 
  imap :: (AImage img px1) => (Int -> Int -> px1 -> px) -> img px1 -> img px
  imap !getPx !img@(dims -> (m, n)) = make m n getNewPx where
    getNewPx !i !j = getPx i j (index img i j)
    {-# INLINE getNewPx #-}
  {-# INLINE imap #-}
  
  -- | Traverse an image.
  traverse :: AImage img px1 =>
              img px1
           -> (Int -> Int -> (Int, Int))
           -> ((Int -> Int -> px1) -> Int -> Int -> px)
           -> img px
  traverse !img@(dims -> (m, n)) getNewDims getNewPx =
    make m' n' (getNewPx (index img)) where
      !(m', n') = getNewDims m n
  {-# INLINE traverse #-}

  -- | Traverse two images.
  traverse2 :: (AImage img px1, AImage img px2) =>
               img px1
            -> img px2
            -> (Int -> Int -> Int -> Int -> (Int, Int))
            -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> Int -> Int -> px)
            -> img px
  traverse2 !img1@(dims -> (m1, n1)) !img2@(dims -> (m2, n2)) !getNewDims !getNewPx =
    make m' n' (getNewPx (index img1) (index img2)) where
      !(m', n') = getNewDims m1 n1 m2 n2
  {-# INLINE traverse2 #-}

  -- | Traverse three images.
  traverse3 :: (AImage img px1, AImage img px2, AImage img px3) =>
               img px1
            -> img px2
            -> img px3
            -> (Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int))
            -> ((Int -> Int -> px1) ->
                (Int -> Int -> px2) ->
                (Int -> Int -> px3) ->
                Int -> Int -> px)
            -> img px
  traverse3 !img1 !img2 !img3 !getNewDims !getNewPx =
    make m' n' (getNewPx (index img1) (index img2) (index img3)) where
      !(m1, n1) = dims img1
      !(m2, n2) = dims img2
      !(m3, n3) = dims img3
      !(m', n') = getNewDims m1 n1 m2 n2 m3 n3
  {-# INLINE traverse3 #-}

  -- | Traverse an image.
  unsafeTraverse :: AImage img px1 =>
              img px1
           -> (Int -> Int -> (Int, Int))
           -> ((Int -> Int -> px1) -> Int -> Int -> px)
           -> img px
  unsafeTraverse !img@(dims -> (m, n)) !getNewDims !getNewPx =
    make m' n' (getNewPx (unsafeIndex img)) where
      !(m', n') = getNewDims m n
  {-# INLINE unsafeTraverse #-}

  -- | UnsafeTraverse two images.
  unsafeTraverse2 :: (AImage img px1, AImage img px2) =>
               img px1
            -> img px2
            -> (Int -> Int -> Int -> Int -> (Int, Int))
            -> ((Int -> Int -> px1) -> (Int -> Int -> px2) -> Int -> Int -> px)
            -> img px
  unsafeTraverse2 !img1@(dims -> (m1, n1)) !img2@(dims -> (m2, n2)) !getNewDims !getNewPx =
    make m' n' (getNewPx (unsafeIndex img1) (unsafeIndex img2)) where
      !(m', n') = getNewDims m1 n1 m2 n2
  {-# INLINE unsafeTraverse2 #-}

  -- | UnsafeTraverse three images.
  unsafeTraverse3 :: (AImage img px1, AImage img px2, AImage img px3) =>
               img px1
            -> img px2
            -> img px3
            -> (Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int))
            -> ((Int -> Int -> px1) ->
                (Int -> Int -> px2) ->
                (Int -> Int -> px3) ->
                Int -> Int -> px)
            -> img px
  unsafeTraverse3 !img1 !img2 !img3 !getNewDims !getNewPx =
    make m' n' (getNewPx (unsafeIndex img1) (unsafeIndex img2) (unsafeIndex img3)) where
      !(m1, n1) = dims img1
      !(m2, n2) = dims img2
      !(m3, n3) = dims img3
      !(m', n') = getNewDims m1 n1 m2 n2 m3 n3
  {-# INLINE unsafeTraverse3 #-}

  -- | Transpose an image
  transpose :: img px
            -> img px
  transpose !img@(dims -> (m, n)) = make n m getPx where
    getPx !i !j = index img j i
    {-# INLINE getPx #-}
  {-# INLINE transpose #-}

  -- | Backpermute an image. Overall size of the image should stay the same.
  backpermute :: Int -> Int -- ^ rows and columns in a new image.
              -> (Int -> Int -> (Int, Int)) -- ^ Function that maps each
                                            -- location in a new image to an old
                                            -- image
              -> img px -- ^ source image
              -> img px
  backpermute !m !n !f !img@(dims -> (m', n')) =
    if m * n /= m' * n'
    then error "backpermute: input image and new dimensions do not agree in size."
    else make m' n' getPx where
      getPx !i !j =  uncurry (index img) $ f i j
      {-# INLINE getPx #-}
  {-# INLINE backpermute #-}

  -- | Crop an image, i.e. retrieves a sub-image image with @m@ rows and @n@
  -- columns. Make sure @(m + i, n + j)@ is not greater than dimensions of a
  -- source image.
  crop :: Int     -- ^ @i@ and 
       -> Int     -- ^ @j@ starting index from within an old image.
       -> Int     -- ^ @m@ rows and
       -> Int     -- ^ @n@ columns - dimensions of a new image.
       -> img px  -- ^ Source image.
       -> img px
  crop !i !j !m !n !img@(dims -> (m', n')) =
    if m + i > m' || n + j > n'
    then error "crop: (m + i, n + j) are greater than old image's dimensions."
    else make m n getPx where
      getPx i' j' = index img (i' + i) (j' + j)
      {-# INLINE getPx #-}
  {-# INLINE crop #-}

  -- | Matrix multiplication of two images. Inner dimensions must agree.
  (|*|) :: img px -> img px -> img px


  fromBoxedVector :: Int -> Int -> V.Vector px -> img px
  
  

maximum :: (Strategy strat img px, AImage img px, Pixel px, Ord px) =>
           strat img px
           -> img px
           -> px
maximum strat img = fold strat max (index img 0 0) img
{-# INLINE maximum #-}

minimum :: (Strategy strat img px, AImage img px, Pixel px, Ord px) =>
           strat img px
           -> img px
           -> px
minimum strat img = fold strat min (index img 0 0) img
{-# INLINE minimum #-}


-- | Retrieves the biggest channel out of all pixels in the image.
biggest :: (Strategy strat img px, AImage img px1, Ord px, Channel px ~ Channel px1) =>
             strat img px -> img px1 -> px
biggest strat img = fold strat max (index biggestImg 0 0) biggestImg  where
  !biggestImg = map (fromChannel . maxChannel) img
{-# INLINE biggest #-}


-- | Retrieves the smallest channel out of all pixels in the image.
smallest :: (Strategy strat img px, AImage img px1, Ord px, Channel px ~ Channel px1) =>
             strat img px -> img px1 -> px
smallest strat img = fold strat min (index smallestImg 0 0) smallestImg  where
  !smallestImg = map (fromChannel . minChannel) img
{-# INLINE smallest #-}


-- | Changes the range of all pixels in the image to values between [0, 1].
normalize :: (Strategy strat img px, Fractional px, Ord px) =>
             strat img px -> img px -> img px
normalize strat img =
  if s == w
  then (if s < 0 then (*0) else if s > 1 then (*1) else id) img
  else map normalizer img where
    !(s, w) = (biggest strat img, smallest strat img)
    normalizer !px = (px - w)/(s - w)
    {-# INLINE normalizer #-}
{-# INLINE normalize #-}

