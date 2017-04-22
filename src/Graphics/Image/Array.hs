{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- |
-- Module      : Graphics.Image.Array
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Array
  ( Array2D(..)
  , Image(..)
  , Strategy(..)
  , makeImageS
  , makeImageP
  , index
  , compute
  ) where

--import           Data.Array.Massiv as M
import           Data.Array.Massiv.Common            as M
import           Data.Array.Massiv.Delayed           as M
import qualified Data.Array.Massiv.Manifest          as M
import           Data.Array.Massiv.Manifest.Storable (S (..))
import           Data.Array.Massiv.Manifest.Unboxed  (U (..), Unbox)
import           Data.Array.Massiv.Mutable           as M
import           Graphics.Image.ColorSpace.Internal
import           Prelude                             as P

-- | Computational strategy: load an image in parallel or sequentially.
data Strategy = Par | Seq

data Image r cs e =
  forall rm. Computable rm =>
             Image Strategy rm !(Array r (Int, Int) (Pixel cs e))


class ( Unbox (Pixel cs e)
      , ColorSpace cs e
      , Source r (Int, Int) (Pixel cs e)
      ) =>
      Array2D r cs e where
  makeImage :: Strategy -> (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image r cs e

instance ColorSpace cs e => Array2D D cs e where

  makeImage s d f = Image s U (makeArray d f)
  {-# INLINE makeImage #-}

instance ColorSpace cs e => Array2D M.M cs e where

  makeImage s d = compute . makeImage s d
  {-# INLINE makeImage #-}

instance ColorSpace cs e => Array2D U cs e where

  makeImage s d f = Image s U (makeArray d f)
  {-# INLINE makeImage #-}

instance ColorSpace cs e => Array2D S cs e where

  makeImage s d f = Image s S (makeArray d f)
  {-# INLINE makeImage #-}




makeImageS :: ColorSpace cs e => (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image D cs e
makeImageS = makeImage Seq
{-# INLINE makeImageS #-}

makeImageP :: ColorSpace cs e => (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image D cs e
makeImageP = makeImage Par
{-# INLINE makeImageP #-}


index :: Image M.M cs e -> (Int, Int) -> Pixel cs e
index (Image _ _ arr) = M.index arr
{-# INLINE index #-}



-- liftArray
--   :: (Array t2 (Int, Int) (Pixel t1 t) -> Array r (Int, Int) (Pixel cs e))
--      -> Image t2 t1 t -> Image r cs e
-- liftArray f (Image s r arr) = Image s r (f arr)

-- mapI
--   :: Array2D r cs1 e1 =>
--      (Pixel cs1 e1 -> Pixel cs e) -> Image r cs1 e1 -> Image D cs e
-- mapI f (Image s r arr) = (Image s r (M.map f arr))


-- mapI'
--   :: (Array2D r cs1 e1, Array2D r cs e) =>
--      (Pixel cs1 e1 -> Pixel cs e) -> Image r cs1 e1 -> Image M cs e
-- mapI' f (Image s r arr) = compute (Image s r (M.map f arr))


class Computable r where

  computeUsing :: Array2D D cs e => r -> Image D cs e -> Image M.M cs e


instance Computable U where

  computeUsing rm (Image Par _ arr) = Image Par rm (M.computeP U arr)
  computeUsing rm (Image Seq _ arr) = Image Seq rm (M.computeS U arr)
  {-# INLINE computeUsing #-}


instance Computable S where

  computeUsing rm (Image Par _ arr) = Image Par rm (M.computeP S arr)
  computeUsing rm (Image Seq _ arr) = Image Seq rm (M.computeS S arr)
  {-# INLINE computeUsing #-}


compute :: ColorSpace cs e =>
           Image D cs e -> Image M.M cs e
compute arr@(Image _ rm _) = computeUsing rm arr
{-# INLINE compute #-}


computeInto
  :: ( Load r1 (Int, Int)
     , Massiv r1 (Int, Int) (Pixel cs e)
     , Computable r
     , Mutable r (Int, Int) (Pixel cs e)
     )
  => r -> Image r1 cs e -> Image r cs e
computeInto r (Image Par _ arr) = Image Par r (M.computeAsP r arr)
computeInto r (Image Seq _ arr) = Image Seq r (M.computeAsS r arr)
{-# INLINE computeInto #-}


toArray :: Image r cs e -> Array r (Int, Int) (Pixel cs e)
toArray (Image _ _ arr) = arr
{-# INLINE toArray #-}
