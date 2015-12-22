{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module HIP.Binary (
  Compareble (..), (.&&.), (.||.), toBinaryImageUsing, toBinaryImageUsing2,
  invert, erode, dialate, open, close, outline4, outline8, distanceTransform
  ) where

import Prelude hiding (map, sum, zipWith)
import Data.Bits ((.&.), (.|.), complement)
import HIP.Interface hiding (maximum, minimum)
import HIP.Algorithms.Convolution
import HIP.Binary.Pixel
import HIP.Pixel.Base (Pixel(..))

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MGV


infix  4  .==., ./=., .<., .<=., .>=., .>.
infixr 3  .&&.
infixr 2  .||.

-- | This is a very convenient set of functions that allow for binary image
-- construction. It is possible to compare either two images of same type
-- pointwise, or an image with an individual pixel, where this pixel will be
-- compared with each pixel in the image. For instance:
class AImage img Binary => Compareble a b img where
  (.==.) :: a -> b -> img Binary  
  (./=.) :: a -> b -> img Binary  
  (.<.)  :: a -> b -> img Binary
  (.<=.) :: a -> b -> img Binary
  (.>.)  :: a -> b -> img Binary
  (.>=.) :: a -> b -> img Binary


instance (Pixel px, Ord px, AImage img px, AImage img Binary)
         => Compareble (img px) (img px) img where
  (.==.) = toBinaryImageUsing2 (==)
  {-# INLINE (.==.) #-}
  
  (./=.) = toBinaryImageUsing2 (/=)
  {-# INLINE (./=.) #-}
  
  (.<.)  = toBinaryImageUsing2 (<)
  {-# INLINE (.<.) #-}
  
  (.<=.) = toBinaryImageUsing2 (<=)
  {-# INLINE (.<=.) #-}
  
  (.>.)  = toBinaryImageUsing2 (>)
  {-# INLINE (.>.) #-}
  
  (.>=.) = toBinaryImageUsing2 (>=)
  {-# INLINE (.>=.) #-}
  

instance (Pixel px, Ord px, AImage img px, AImage img Binary)
         => Compareble px (img px) img where
  (.==.) !px = toBinaryImageUsing (==px)
  {-# INLINE (.==.) #-}
  
  (./=.) !px = toBinaryImageUsing (/=px)
  {-# INLINE (./=.) #-}
  
  (.<.)  !px = toBinaryImageUsing (< px)
  {-# INLINE (.<.) #-}
  
  (.<=.) !px = toBinaryImageUsing (<=px)
  {-# INLINE (.<=.) #-}
  
  (.>.)  !px = toBinaryImageUsing (> px)
  {-# INLINE (.>.) #-}
  
  (.>=.) !px = toBinaryImageUsing (>=px)
  {-# INLINE (.>=.) #-}
  

instance (Pixel px, Ord px, AImage img px, AImage img Binary)
         => Compareble (img px) px img where
  (.==.) !img !px = toBinaryImageUsing (==px) img
  {-# INLINE (.==.) #-}
  
  (./=.) !img !px = toBinaryImageUsing (/=px) img
  {-# INLINE (./=.) #-}
  
  (.<.)  !img !px = toBinaryImageUsing (< px) img
  {-# INLINE (.<.) #-}
  
  (.<=.) !img !px = toBinaryImageUsing (<=px) img
  {-# INLINE (.<=.) #-}
  
  (.>.)  !img !px = toBinaryImageUsing (> px) img
  {-# INLINE (.>.) #-}
  
  (.>=.) !img !px = toBinaryImageUsing (>=px) img
  {-# INLINE (.>=.) #-}
  

-- | Pixel wise @AND@ operator on binary images. 
(.&&.) :: AImage img Binary => img Binary -> img Binary -> img Binary
(.&&.) = zipWith (.&.)
{-# INLINE (.&&.) #-}

-- | Pixel wise @OR@ operator on binary images.
(.||.) :: AImage img Binary => img Binary -> img Binary -> img Binary
(.||.) = zipWith (.|.)
{-# INLINE (.||.) #-}


toBinaryImageUsing :: (AImage img px, AImage img Binary) =>
            (px -> Bool)
         -> img px
         -> img Binary
toBinaryImageUsing !f = map (Binary . f)
{-# INLINE toBinaryImageUsing #-}


toBinaryImageUsing2 :: (AImage img px, AImage img Binary) =>
             (px -> px -> Bool)
          -> img px
          -> img px
          -> img Binary
toBinaryImageUsing2 !f =  zipWith (((.).(.)) Binary f)
{-# INLINE toBinaryImageUsing2 #-}
  

invert :: AImage img Binary => img Binary -> img Binary
invert = map complement
{-# INLINE invert #-}


erode :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
         strat img Binary -> img Binary -> img Binary -> img Binary
erode strat !struc !img = 
  compute strat (invert ((convolve (Fill on) struc (invert img)) ./=. off))
{-# INLINE erode #-}


dialate :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
           strat img Binary -> img Binary -> img Binary -> img Binary
dialate strat !struc !img =
  compute strat (convolve (Fill off) struc img) ./=. off
{-# INLINE dialate #-}


open :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
        strat img Binary -> img Binary -> img Binary -> img Binary
open strat struc = dialate strat struc . erode strat struc
{-# INLINE open #-}


close :: (Compareble (img Binary) Binary img, Strategy strat img Binary) =>
        strat img Binary -> img Binary -> img Binary -> img Binary
close strat struc = erode strat struc . dialate strat struc
{-# INLINE close #-}


outline' :: AImage img Binary => (Int -> Int -> [(Int, Int)]) -> img Binary -> img Binary
outline' getIdxs img@(dims -> (m, n)) = imap initPx img where
  initPx i j px | i `mod` (m-1) /= 0 && j `mod` (n-1) /= 0 &&
                  any ((/= px) . uncurry (unsafeIndex img)) (getIdxs i j) = on
                | otherwise = off
{-# INLINE outline' #-}


outline4 :: AImage img Binary => img Binary -> img Binary
outline4 = outline' (\i j -> [(i, j+1), (i+1, j), (i, j-1), (i-1, j)])
{-# INLINE outline4 #-}


outline8 :: AImage img Binary => img Binary -> img Binary
outline8 = outline' (\i j -> [(i, j+1), (i+1, j), (i, j-1), (i-1, j),
                              (i+1, j+1), (i-1, j+1), (i+1, j-1), (i-1, j-1)])
{-# INLINE outline8 #-}


chamfer5x5 m n mv = do
  let maskV = [(-2, -1, 11), (-2, 1, 11), (-1, -2, 11), (-1, -1, 7),
                (0, -1, 5), (-1, 1, 7), (-1, 2, 11), (0, -1, 5)]
      maskR k = [((i + i') * n + j + j', d) | (i', j', d) <- maskV,
                 i+i' >= 0, i+i' < m, (j+j') >= 0, (j+j') < n] where
        (i, j) = (k `div` n, k `mod` n)
      maskL k = [((i - i') * n + j - j', d) | (i', j', d) <- maskV,
                 i-i' >= 0, i-i' < m, (j-j') >= 0, (j-j') < n] where
        (i, j) = (k `div` n, k `mod` n)
                                                                 
  let calc vec mask k = do
        let readSum (k', d) = do
              px' <- MGV.read vec k'
              return $ px' + d
        ns <- mapM readSum (mask k)
        px <- MGV.read vec k
        MGV.write vec k $ minimum (px:ns)
        return px
          
  mapM_ (calc mv maskR) [0..(m*n-1)]
  mapM_ (calc mv maskL) [(m*n-1),(m*n-2)..0]


--mapM f img = return $ map f img

-- | Given a binary image, distanceTransform returns an image representing the
-- 2D distance transform of the image. The distance transform uses Chamfer's 5x5
-- algorithm and is accurate to within a 2% error for euclidean distance.
distanceTransform :: (Strategy strat img Int, AImage img Binary, AImage img Int) =>
                     strat img Int -> img Binary -> img Int
distanceTransform strat img@(dims -> (m, n)) =
  fromBoxedVector m n $ GV.modify (chamfer5x5 m n) $ toBoxedVector strat o where
    o = map (\x -> if isOff x then maxBound `div` 2 else 0) $ outline4 img
    --initPx i j | i `mod` (m-1) /= 0 && j `mod` (n-1) /= 0 &&
    --             any ((/= unsafeIndex img i j) . (uncurry (unsafeIndex img)))
    --             [(i, j+1), (i+1, j), (i, j-1), (i-1, j)] = 0
    --           | otherwise = maxBound `div` 2
    
    
{-# INLINE distanceTransform #-}


