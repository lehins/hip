{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Graphics.Image.Interface
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Internal
  ( Image(..)
  , dims
  , makeImage
  , makeImageWindowed
  , scalar
  , index00
  , map
  , imap
  , zipWith
  , izipWith
  , traverse
  , traverse2
  , transpose
  , backpermute
  , fromLists
  , fold
  , foldIx
  , (|*|)
  , index
  , unsafeIndex
  , defaultIndex
  , maybeIndex
  , borderIndex
  , handleBorderIndex
  , Border(..)
  , compute
  , toVector
  , fromVector
  , exchange
  , mapM
  , mapM_
  ) where

import           Control.DeepSeq          (NFData (rnf))
import           Data.Vector.Generic      as VG (convert)
import           Graphics.Image.Interface
import           Prelude                  hiding (map, mapM, mapM_, traverse,
                                           zipWith)
import Data.Array.Massiv


data Strat = Par | Seq

newtype Image arr cs e = DImage Strat arr (Array D (Int, Int) (Pixel cs e))
                       | MImage Strat (Array arr (Int, Int) (Pixel cs e))




instance Array arr cs e => Eq (Image arr cs e) where
  (Image arr1) == (Image arr2) = arr1 == arr2


instance (BaseArray arr (Int, Int) (Pixel cs e)) => Show (Image arr cs e) where
  show (Image arr) = let !(m, n) = shapeA arr in
    "<Image " ++ show m ++ "x" ++ show n ++ ">"


instance NFData (arr (Int, Int) (Pixel cs e)) =>
         NFData (Image arr cs e) where
  rnf (Image arr) = rnf arr

instance Array arr cs e => Num (Image arr cs e) where
  (+)         = zipWith (+)
  {-# INLINE (+) #-}
  (-)         = zipWith (-)
  {-# INLINE (-) #-}
  (*)         = zipWith (*)
  {-# INLINE (*) #-}
  abs         = map abs
  {-# INLINE abs #-}
  signum      = map signum
  {-# INLINE signum #-}
  fromInteger = scalar . fromInteger
  {-# INLINE fromInteger #-}

instance (Fractional (Pixel cs e), Array arr cs e) =>
         Fractional (Image arr cs e) where
  (/)          = zipWith (/)
  {-# INLINE (/) #-}
  fromRational = scalar . fromRational
  {-# INLINE fromRational #-}


instance (Floating (Pixel cs e), Array arr cs e) =>
         Floating (Image arr cs e) where
  pi    = scalar pi
  {-# INLINE pi #-}
  exp   = map exp
  {-# INLINE exp #-}
  log   = map log
  {-# INLINE log #-}
  sin   = map sin
  {-# INLINE sin #-}
  cos   = map cos
  {-# INLINE cos #-}
  asin  = map asin
  {-# INLINE asin #-}
  atan  = map atan
  {-# INLINE atan #-}
  acos  = map acos
  {-# INLINE acos #-}
  sinh  = map sinh
  {-# INLINE sinh #-}
  cosh  = map cosh
  {-# INLINE cosh #-}
  asinh = map asinh
  {-# INLINE asinh #-}
  atanh = map atanh
  {-# INLINE atanh #-}
  acosh = map acosh
  {-# INLINE acosh #-}

-- | Get dimensions of an image.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> frog
-- <Image VectorUnboxed RGB (Double): 200x320>
-- >>> dims frog
-- (200,320)
--
dims :: BaseArray arr (Int,Int) (Pixel cs e) => Image arr cs e -> (Int, Int)
dims (Image arr) = shapeA arr
{-# INLINE dims #-}


-- | Create an Image by supplying it's dimensions and a pixel generating
-- function.
makeImage
  :: Array arr cs e
  => (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
  -> ((Int, Int) -> Pixel cs e)
             -- ^ A function that takes (@i@-th row, and @j@-th column) as an
             -- argument and returns a pixel for that location.
  -> Image arr cs e
makeImage !sz = Image . makeA sz
{-# INLINE makeImage #-}


-- | Create an image using two different genrating functions, one for interior
-- of the image, i.e. the window, and another one for the border.
makeImageWindowed :: Array arr cs e =>
                     (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image
                  -> (Int, Int) -- ^ Starting index of the window
                  -> (Int, Int) -- ^ Size of the window
                  -> ((Int, Int) -> Pixel cs e)
                     -- ^ Function that generates inner pixels
                  -> ((Int, Int) -> Pixel cs e)
                     -- ^ Function that generates border pixels
                  -> Image arr cs e
makeImageWindowed sz ix0 szW fW fB = Image (makeWindowedA sz ix0 szW fW fB)
{-# INLINE makeImageWindowed #-}


-- | Create a scalar image, i.e. one that contains a single pixel.
scalar :: Array arr cs e => Pixel cs e -> Image arr cs e
scalar = Image . scalarA
{-# INLINE scalar #-}


-- | Retrieves a pixel at @(0, 0)@ index. Useful together with `fold`, when
-- arbitrary initial pixel is needed.
index00 :: Array arr cs e => Image arr cs e -> Pixel cs e
index00 (Image arr) = unsafeIndexA arr (0,0)
{-# INLINE index00 #-}

  -- | Map a function over a an image.
map :: (Array arr cs1 e1, Array arr cs e) =>
       (Pixel cs1 e1 -> Pixel cs e)
       -- ^ A function that takes a pixel of a source image and returns a pixel
       -- for the result image a the same location.
    -> Image arr cs1 e1 -- ^ Source image.
    -> Image arr cs e   -- ^ Result image.
map f (Image arr) = Image (mapA f arr)
{-# INLINE map #-}


-- | Map an index aware function over each pixel in an image.
imap :: (Array arr cs1 e1, Array arr cs e) =>
        ((Int, Int) -> Pixel cs1 e1 -> Pixel cs e)
        -- ^ A function that takes an index @(i, j)@, a pixel at that location
        -- and returns a new pixel at the same location for the result image.
     -> Image arr cs1 e1 -- ^ Source image.
     -> Image arr cs e   -- ^ Result image.
imap f (Image arr) = Image (imapA f arr)
{-# INLINE imap #-}


-- | Zip two images with a function
zipWith :: (Array arr cs1 e1, Array arr cs2 e2, Array arr cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e
zipWith f (Image arr1) (Image arr2) = Image (zipWithA f arr1 arr2)
{-# INLINE zipWith #-}

-- | Zip two images with an index aware function
izipWith :: (Array arr cs1 e1, Array arr cs2 e2, Array arr cs e) =>
            ((Int, Int) -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
         -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e
izipWith f (Image arr1) (Image arr2) = Image (izipWithA f arr1 arr2)
{-# INLINE izipWith #-}

-- | Traverse an image
traverse :: (Array arr cs1 e1, Array arr cs e) =>
            Image arr cs1 e1 -- ^ Source image.
         -> ((Int, Int) -> (Int, Int))
         -- ^ Function that takes dimensions of a source image and returns
         -- dimensions of a new image.
         -> (((Int, Int) -> Pixel cs1 e1) -> (Int, Int) -> Pixel cs e)
         -- ^ Function that receives a pixel getter (a source image index
         -- function), a location @(i, j)@ in a new image and returns a pixel
         -- for that location.
         -> Image arr cs e
traverse (Image arr) newDims newPx = Image (traverseA arr newDims newPx)
{-# INLINE traverse #-}

-- | Traverse two images.
traverse2 :: (Array arr cs1 e1, Array arr cs2 e2, Array arr cs e) =>
             Image arr cs1 e1 -- ^ First source image.
          -> Image arr cs2 e2 -- ^ Second source image.
          -> ((Int, Int) -> (Int, Int) -> (Int, Int))
          -- ^ Function that produces dimensions for the new image.
          -> (((Int, Int) -> Pixel cs1 e1) -> ((Int, Int) -> Pixel cs2 e2) -> (Int, Int) -> Pixel cs e)
          -- ^ Function that produces pixels for the new image.
          -> Image arr cs e
traverse2 (Image arr1) (Image arr2) newDims newPx = Image (traverse2A arr1 arr2 newDims newPx)
{-# INLINE traverse2 #-}

-- | Transpose an image
transpose :: Array arr cs e => Image arr cs e -> Image arr cs e
transpose (Image arr) = Image (transposeA arr)
{-# INLINE transpose #-}

-- | Backwards permutation of an image.
backpermute :: Array arr cs e =>
               (Int, Int) -- ^ Dimensions of a result image.
            -> ((Int, Int) -> (Int, Int))
               -- ^ Function that maps an index of a source image to an index
               -- of a result image.
            -> Image arr cs e -- ^ Source image.
            -> Image arr cs e -- ^ Result image.
backpermute !sz f (Image arr) = Image (backpermuteA sz f arr)
{-# INLINE backpermute #-}

-- | Construct an image from a nested rectangular shaped list of pixels.
fromLists :: Array arr cs e => [[Pixel cs e]] -> Image arr cs e
fromLists = Image . fromListsA
{-# INLINE fromLists #-}



-- | Perform matrix multiplication on two images. Inner dimensions must agree.
(|*|) :: (Num (Pixel cs e), Array arr cs e) => Image arr cs e -> Image arr cs e -> Image arr cs e
(|*|) (Image arr1) (Image arr2) = Image (multA arr1 arr2)
{-# INLINE (|*|) #-}


-- | Undirected reduction of an image.
fold :: Array arr cs e =>
        (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
     -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
     -> Image arr cs e -- ^ Source image.
     -> Pixel cs e
fold f !px0 (Image arr) = foldA f px0 arr
{-# INLINE fold #-}


-- | Undirected reduction of an image with an index aware function.
foldIx :: Array arr cs e =>
          (Pixel cs e -> (Int, Int) -> Pixel cs e -> Pixel cs e)
          -- ^ Function that takes an accumulator, index, a pixel at that
          -- index and returns a new accumulator pixel.
       -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
       -> Image arr cs e -- ^ Source image.
       -> Pixel cs e
foldIx f !px0 (Image arr) = foldIxA f px0 arr
{-# INLINE foldIx #-}



-- | Approach to be used near the borders during various transformations.
-- Whenever a function needs information not only about a pixel of interest, but
-- also about it's neighbours, it will go out of bounds around the image edges,
-- hence is this set of approaches that can be used in such situtation.
data Border px =
  Fill !px    -- ^ Fill in a constant pixel.
              --
              -- @
              --            outside |  Image  | outside
              -- ('Fill' 0) : 0 0 0 0 | 1 2 3 4 | 0 0 0 0
              -- @
              --
  | Wrap      -- ^ Wrap around from the opposite border of the image.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Wrap' :     1 2 3 4 | 1 2 3 4 | 1 2 3 4
              -- @
              --
  | Edge      -- ^ Replicate the pixel at the edge.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Edge' :     1 1 1 1 | 1 2 3 4 | 4 4 4 4
              -- @
              --
  | Reflect   -- ^ Mirror like reflection.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Reflect' :  4 3 2 1 | 1 2 3 4 | 4 3 2 1
              -- @
              --
  | Continue  -- ^ Also mirror like reflection, but without repeating the edge pixel.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Continue' : 1 4 3 2 | 1 2 3 4 | 3 2 1 4
              -- @
              --
  deriving Show


-- | Border handling function. If @(i, j)@ location is within bounds, then supplied
-- lookup function will be used, otherwise it will be handled according to a
-- supplied border strategy.
handleBorderIndex :: Border px -- ^ Border handling strategy.
                   -> (Int, Int) -- ^ Image dimensions
                   -> ((Int, Int) -> px) -- ^ Image's indexing function.
                   -> (Int, Int) -- ^ @(i, j)@ location of a pixel lookup.
                   -> px
handleBorderIndex ~border !(m, n) getPx !(i, j) =
  if north || east || south || west
  then case border of
    Fill px  -> px
    Wrap     -> getPx (i `mod` m, j `mod` n)
    Edge     -> getPx (if north then 0 else if south then m - 1 else i,
                       if west then 0 else if east then n - 1 else j)
    Reflect  -> getPx (if north then (abs i - 1) `mod` m else
                         if south then (-i - 1) `mod` m else i,
                       if west then (abs j - 1) `mod` n else
                         if east then (-j - 1) `mod` n else j)
    Continue -> getPx (if north then abs i `mod` m else
                         if south then (-i - 2) `mod` m else i,
                       if west then abs j `mod` n else
                         if east then (-j - 2) `mod` n else j)
  else getPx (i, j)
  where
    !north = i < 0
    !south = i >= m
    !west  = j < 0
    !east  = j >= n
{-# INLINE handleBorderIndex #-}


-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> index grad_gray (20, 30) == PixelY ((20*30) / (200*200))
-- True
--
index :: MArray arr cs e => Image arr cs e -> (Int, Int) -> Pixel cs e
index !img !ix =
  borderIndex (error $ show img ++ " - Index out of bounds: " ++ show ix) img ix
{-# INLINE index #-}

unsafeIndex :: MArray arr cs e => Image arr cs e -> (Int, Int) -> Pixel cs e
unsafeIndex !(Image arr) !ix = unsafeIndexA arr ix
{-# INLINE unsafeIndex #-}


-- | Image indexing function that returns a default pixel if index is out of bounds.
defaultIndex :: MArray arr cs e =>
                Pixel cs e -> Image arr cs e -> (Int, Int) -> Pixel cs e
defaultIndex !px !img@(Image arr) = handleBorderIndex (Fill px) (shapeA arr) (index img)
{-# INLINE defaultIndex #-}


-- | Image indexing function that returns @'Nothing'@ if index is out of bounds,
-- @'Just' px@ otherwise.
maybeIndex :: MArray arr cs e =>
              Image arr cs e -> (Int, Int) -> Maybe (Pixel cs e)
maybeIndex (Image arr) !(i, j) = let !(m, n) = shapeA arr in
  if i >= 0 && j >= 0 && i < m && j < n then Just $ unsafeIndexA arr (i, j) else Nothing
{-# INLINE maybeIndex #-}


-- | Image indexing function that uses a special border resolutions strategy for
-- out of bounds pixels.
borderIndex :: MArray arr cs e =>
               Border (Pixel cs e) -> Image arr cs e -> (Int, Int) -> Pixel cs e
borderIndex ~atBorder (Image arr) = handleBorderIndex atBorder (shapeA arr) (unsafeIndexA arr)
{-# INLINE borderIndex #-}



-- | `Array` class does not enforce an image to be represented as concrete
-- array of pixels in memory, but if at any time it is desired for the image
-- to be brought to a computed state, this function can be used.
compute :: Array arr cs e => Image arr cs e -> Image arr cs e
compute (Image arr) = Image (computeA arr)
{-# INLINE compute #-}


-- | Convert an image to a flattened 'Vector'. For all current representations
-- it is a __O(1)__ opeartion.
--
-- >>> toVector $ makeImage (3, 2) (\(i, j) -> PixelY $ fromIntegral (i+j))
-- fromList [<Luma:(0.0)>,<Luma:(1.0)>,<Luma:(1.0)>,<Luma:(2.0)>,<Luma:(2.0)>,<Luma:(3.0)>]
--
toVector :: Array arr cs e => Image arr cs e -> Vector arr (Pixel cs e)
toVector (Image arr) = toVectorA arr
{-# INLINE toVector #-}

-- | Construct a two dimensional image with @m@ rows and @n@ columns from a
--  flat 'Vector' of length @k@. For all current representations it is a
--  __O(1)__ opeartion. Make sure that @m * n = k@.
--
-- >>> fromVector (200, 300) $ generate 60000 (\i -> PixelY $ fromIntegral i / 60000)
-- <Image Vector Luma: 200x300>
--
-- <<images/grad_fromVector.png>>
--
fromVector :: Array arr cs e => (Int, Int) -> Vector arr (Pixel cs e) -> Image arr cs e
fromVector !sz v = Image (fromVectorA sz v)
{-# INLINE fromVector #-}


-- | Exchange the underlying array representation of an image.
exchange :: (Array arr' cs e, Array arr cs e) =>
            Repr arr -- ^ New representation of an image.
         -> Image arr' cs e -- ^ Source image.
         -> Image arr cs e
exchange _ img = fromVector (dims img) $ VG.convert $ toVector img
{-# INLINE[1] exchange #-}

{-# RULES
"exchange/id" forall arr. exchange arr = id
 #-}


mapM :: (Monad m, MArray arr cs1 e1, MArray arr cs e) =>
        (Pixel cs1 e1 -> m (Pixel cs e))
     -> Image arr cs1 e1
     -> m (Image arr cs e)
mapM f (Image arr) = Image <$> mapMA f arr



mapM_ :: (Monad m, MArray arr cs e) =>
         (Pixel cs e -> m b)
      -> Image arr cs e
      -> m ()
mapM_ f (Image arr) = mapM_A f arr
