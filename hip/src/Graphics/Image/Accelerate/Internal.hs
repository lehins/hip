{-# LANGUAGE GADTs, ConstraintKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, ViewPatterns, UndecidableInstances #-}
module Graphics.Image.Accelerate.Internal (
  A,
  ) where

import Graphics.Image.Interface
import GHC.Exts (Constraint)
import Prelude hiding (map, zipWith)
import Graphics.Image.Interface

import Data.Array.Accelerate hiding (Array, Elt, map, zipWith, index, gather, length)
import Data.Array.Accelerate.Interpreter
import qualified Data.Array.Accelerate as A

import Graphics.Image.ColorSpace.RGB (RGB(..), Pixel(..))


-- | Accelerate Array
data A
  
instance (Elt A cs (Exp e)) => Array A cs (Exp e) where
  type Ix A = Exp Int
  type S A c = Exp c
  type Elt A cs (Exp e) = (Lift Exp (PixelElt cs e),
                         Plain (PixelElt cs (Exp e)) ~ PixelElt cs e,
                         Unlift Exp (PixelElt cs (Exp e)),
                         IsNum e, ColorSpace cs, Num (Pixel cs (Exp e)),
                         A.Elt e, A.Elt (PixelElt cs e))
                        
  data Image A cs (Exp e) where
    AccImageSc :: Acc (A.Vector e)                   -> Image A cs (Exp e)
    AccImageCh :: Acc (A.Array DIM3 e)               -> Image A cs (Exp e)
    AccImagePx :: Acc (A.Array DIM2 (PixelElt cs e)) -> Image A cs (Exp e)

  dims (AccImageSc _)                                                     = (1, 1)
  dims (AccImageCh ((unlift . shape) -> (Z :. (_ :: Exp Int) :. m :. n))) = (m, n)
  dims (AccImagePx ((unlift . shape) -> (Z :. m :. n)))                   = (m, n)

  index (AccImageSc arr) c _      = arr ! (index1 c)
  index (AccImageCh arr) c (i, j) = arr ! (index3 c i j)
  index (AccImagePx arr) c (i, j) =
    getEltCh cond (==*) (unlift (arr ! (index2 i j))) (undefined :: cs) c

  indexPx (AccImagePx arr) (i, j) = fromElt $ unlift (arr ! (index2 i j))
  indexPx img              ix     = indexPx img ix

  indexElt (AccImagePx arr) (i, j) = unlift (arr ! (index2 i j))
  indexElt img              ix     = toElt $ indexPx img ix

  singleton px = AccImageSc (generate (index1 (pixelRank px)) getCh') where
    getCh' (unlift -> (Z :. c)) = getPxCh cond (==*) px c

  make (m, n) f = AccImagePx $ generate (index2 m n) getPx' where
    getPx' (unlift -> (Z :. i :. j)) = lift . toElt . f $ (i, j)

  map f (AccImageSc arr) = AccImageSc $ A.map f arr
  map f (AccImageCh arr) = AccImageCh $ A.map f arr
  map f img              = map f (disperse img)
  
  imap f (AccImageSc arr) =
    (AccImageSc (generate (shape arr) getCh') :: Image A cs (Exp e)) where
      getCh' sh@(unlift -> (Z :. c)) = f (0, 0) c (arr ! sh)
  imap f (AccImageCh arr) =
    (AccImageCh (generate (shape arr) getCh') :: Image A cs (Exp e)) where
      getCh' sh@(unlift -> (Z :. c :. i :. j)) =
        f (i, j) c (arr ! sh)
  imap f img = imap f (disperse img)
  
  mapElt f img@(AccImageSc _) = singleton . fromElt . f $ indexElt img (0, 0)
  mapElt f (AccImagePx arr)   = AccImagePx (A.map (lift1 f) arr) 
  mapElt f img                = mapElt f (group img)

  mapPx f img@(AccImageSc _) = singleton . f $ indexPx img (0, 0)
  mapPx f img                = mapElt (toElt . f . fromElt) img

  zipWith f (AccImageSc arr1)   (AccImageSc arr2)   = AccImageSc $ A.zipWith f arr1 arr2
  zipWith f (AccImageCh arr1)   (AccImageCh arr2)   = AccImageCh $ A.zipWith f arr1 arr2
  zipWith f (AccImageSc arr1)   (AccImageCh arr2)   = AccImageCh $ generate (shape arr2) getCh'
    where getCh' sh@(unlift -> (Z :. c :. (_ :: Exp Int) :. (_ :: Exp Int))) =
            f (arr1 ! (index1 c)) (arr2 ! sh)
  zipWith f img1@(AccImageCh _) img2@(AccImageSc _) = zipWith (flip f) img2 img1
  zipWith f img1                img2                = zipWith f (disperse img1) (disperse img2)

  zipWithElt f img1@(AccImageSc _) img2@(AccImageSc _) =
    singleton $ fromElt $ f (indexElt img1 (0, 0)) (indexElt img2 (0, 0))
  zipWithElt f img1@(AccImageSc _) img2@(AccImagePx _) =
    mapElt (f (toElt $ indexPx img1 (0, 0))) img2    
  zipWithElt f img1@(AccImagePx _) img2@(AccImageSc _) =
    mapElt (`f` (indexElt img2 (0, 0))) img1
  zipWithElt f (AccImagePx arr1)   (AccImagePx arr2)   =
    AccImagePx $ A.zipWith (lift2 f) arr1 arr2
  zipWithElt f img1                img2                =
    zipWithElt f (group img1) (group img2)

  zipWithPx f = zipWithElt func where
    func elt1 elt2 = toElt $ f (fromElt elt1) (fromElt elt2)
  
  group img@(AccImageCh ((unlift . shape) -> (Z :. (_ :: Exp Int) :. m :. n))) =
    AccImagePx $ generate (index2 m n) getElt' where
      getElt' (unlift -> (Z :. i :. j)) = lift $ indexElt' img (undefined :: cs) (i, j)
  group img = img

  disperse (AccImagePx arr) =
    AccImageCh $ generate (index3 (rank (undefined :: cs)) m n) getCh' where
      (Z :. m :. n) = unlift . shape $ arr
      getCh' (unlift -> (Z :. c :. i :. j)) =
        getEltCh cond (==*) (unlift (arr ! (index2 i j))) (undefined :: cs) c
  disperse img = img




           
--compute' (AccImageSc arr) = run arr
--compute' (AccImageCh arr) = run arr
compute' (AccImagePx arr) = run arr



index3 :: (Slice (Z :. i :. i), Slice (Z :. i), A.Elt i) =>
          Exp i -> Exp i -> Exp i -> Exp (Z :. i :. i :. i)
index3 i j k = lift (Z :. i :. j :. k)  



