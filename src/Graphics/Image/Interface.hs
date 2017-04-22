{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE ViewPatterns            #-}
{-# LANGUAGE GADTs            #-}
#if __GLASGOW_HASKELL__ >= 800
    {-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module      : Graphics.Image.Interface
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface (
  -- * Pixel and ColorSpace
  Pixel
  , ColorSpace(..)
  , AlphaSpace(..)
  , Elevator(..)
  -- -- * Array and Image
  -- , BaseArray(..)
  -- , IArray(..)
  -- , Array(..)
  -- , Repr
  -- , Vector
  -- -- * MArray and MImage
  -- , ManifestArray(..)
  -- -- , MutableArray(..)
  -- -- , MA
  -- , MArray
  -- createImage,
  -- -- * Exchanging Representation
  -- exchange,
  -- -- * Indexing
  -- index, defaultIndex, borderIndex, maybeIndex, Border(..), handleBorderIndex,
  -- * Tools
  --fromIx, toIx, checkDims
#if !MIN_VERSION_base(4,8,0)
  , module Control.Applicative
  , Foldable
#endif
  ) where

import           Prelude                           hiding (and, map, product,
                                                    sum, zipWith)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.DeepSeq                   (NFData (rnf), deepseq)
-- import           Control.Monad.Primitive           (PrimMonad (..))
-- import           Control.Monad.ST
import           Data.Foldable
import           Data.Maybe                        (fromMaybe)
-- import           Data.Proxy                        (Proxy (..))
import           Data.Typeable                     (Typeable)
-- , showsTypeRep,
--                                                     typeRep)


import qualified Data.Vector.Unboxed               as VU


import           Graphics.Image.Interface.Elevator

-- | A Pixel family with a color space and a precision of elements.
data family Pixel cs e :: *


class (Eq cs, Enum cs, Show cs, Bounded cs, Typeable cs,
      Eq (Pixel cs e), VU.Unbox (Components cs e), Elevator e)
      => ColorSpace cs e where

  type Components cs e

  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Pixel cs e -> Components cs e

  -- | Convert from an elemnt representation back to a Pixel.
  fromComponents :: Components cs e -> Pixel cs e

  -- | Construt a Pixel by replicating the same value across all of the components.
  promote :: e -> Pixel cs e

  -- | Retrieve Pixel's component value
  getPxC :: Pixel cs e -> cs -> e

  -- | Set Pixel's component value
  setPxC :: Pixel cs e -> cs -> e -> Pixel cs e

  -- | Map a channel aware function over all Pixel's components.
  mapPxC :: (cs -> e -> e) -> Pixel cs e -> Pixel cs e

  -- | Map a function over all Pixel's componenets.
  liftPx :: (e -> e) -> Pixel cs e -> Pixel cs e

  -- | Zip two Pixels with a function.
  liftPx2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e

  -- | Left fold on two pixels a the same time.
  foldlPx2 :: (b -> e -> e -> b) -> b -> Pixel cs e -> Pixel cs e -> b

  -- | Right fold over all Pixel's components.
  foldrPx :: (e -> b -> b) -> b -> Pixel cs e -> b
  foldrPx f !z0 !xs = foldlPx f' id xs z0
      where f' k x !z = k $! f x z

  -- | Left strict fold over all Pixel's components.
  foldlPx :: (b -> e -> b) -> b -> Pixel cs e -> b
  foldlPx f !z0 !xs = foldrPx f' id xs z0
      where f' x k !z = k $! f z x

  foldl1Px :: (e -> e -> e) -> Pixel cs e -> e
  foldl1Px f !xs = fromMaybe (error "foldl1Px: empty Pixel")
                  (foldlPx mf Nothing xs)
      where
        mf m !y = Just (case m of
                           Nothing -> y
                           Just x  -> f x y)
  toListPx :: Pixel cs e -> [e]
  toListPx !px = foldr' f [] (enumFrom (toEnum 0))
    where f !cs !ls = getPxC px cs:ls



-- | A color space that supports transparency.
class (ColorSpace (Opaque cs) e, ColorSpace cs e) => AlphaSpace cs e where
  -- | A corresponding opaque version of this color space.
  type Opaque cs

  -- | Get an alpha channel of a transparant pixel.
  getAlpha :: Pixel cs e -> e

  -- | Add an alpha channel to an opaque pixel.
  --
  -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e

  -- | Convert a transparent pixel to an opaque one by dropping the alpha
  -- channel.
  --
  -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
  --
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


-- data family Repr (a :: * -> * -> *) :: *

-- type family Vector (a :: * -> * -> *) :: * -> *


-- class SuperClass a i e => BaseArray (a :: * -> * -> *) i e where

--   -- | Required array specific constraints for an array element.
--   type SuperClass a i e :: Constraint

--   shapeA :: a i e -> i

--   makeA :: i -> (i -> e) -> a i e

--   unsafeIndexA :: a i e -> i -> e


-- -- | Immutable, shape polymorphic array representation.
-- class BaseArray a i e => IArray a i e where

--   makeWindowedA :: i -> i -> i -> (i -> e) -> (i -> e) -> a i e

--   scalarA :: e -> a i e

--   mapA :: IArray a i e1 => (e1 -> e) -> a i e1 -> a i e

--   imapA :: IArray a i e1 => (i -> e1 -> e) -> a i e1 -> a i e

--   zipWithA :: (IArray a i e1, IArray a i e2) => (e1 -> e2 -> e) -> a i e1 -> a i e2 -> a i e

--   izipWithA :: (IArray a i e1, IArray a i e2) => (i -> e1 -> e2 -> e) -> a i e1 -> a i e2 -> a i e

--   traverseA :: IArray a i e1 => a i e1 -> (i -> i) -> ((i -> e1) -> i -> e) -> a i e

--   traverse2A :: (IArray a i e1, IArray a i e2) =>
--     a i e1 -> a i e2 -> (i -> i -> i) -> ((i -> e1) -> (i -> e2) -> i -> e) -> a i e

--   transposeA :: a i e -> a i e

--   backpermuteA :: i -> (i -> i) -> a i e -> a i e

--   fromListsA :: [[e]] -> a i e

--   foldA :: (e -> e -> e) -> e -> a i e -> e

--   foldIxA :: (e -> i -> e -> e) -> e -> a i e -> e

--   multA :: Num e => a i e -> a i e -> a i e

--   computeA :: a i e -> a i e



-- class BaseArray a i e => ManifestArray a i e where

--   foldlA :: (b -> e -> b) -> b -> a i e -> b

--   foldrA :: (e -> b -> b) -> b -> a i e -> b

--   makeArrayMA :: (Functor m, Monad m) => i -> (i -> m e) -> m (a i e)

--   mapMA :: (ManifestArray a i e1, Functor m, Monad m) =>
--     (e1 -> m e) -> a i e1 -> m (a i e)

--   mapM_A :: (Functor m, Monad m) => (e -> m b) -> a i e -> m ()

--   foldMA :: (Functor m, Monad m) => (b -> e -> m b) -> b -> a i e -> m b

--   foldM_A :: (Functor m, Monad m) => (b -> e -> m b) -> b -> a i e -> m ()


-- type family MA (a :: * -> * -> *) :: * -> * -> * -> *


-- class MutableArray a i e where

--   -- | Yield a mutable copy of an array.
--   thawA :: (Functor m, PrimMonad m) => a i e -> m ((MA a) (PrimState m) i e)

--   -- | Yield an immutable copy of an array.
--   freezeA :: (Functor m, PrimMonad m) =>
--             (MA a) (PrimState m) i e -> m (a i e)

--   -- | Create a mutable array with given dimensions.
--   mshapeA :: (MA a) s i e -> i

--   -- | Create a mutable array with given dimensions.
--   newA :: (Functor m, PrimMonad m) => i -> m ((MA a) (PrimState m) i e)

--   -- | Yield the pixel at a given location.
--   readA :: (Functor m, PrimMonad m) => (MA a) (PrimState m) i e -> i -> m e

--   -- | Set a pixel at a given location.
--   writeA :: (Functor m, PrimMonad m) => (MA a) (PrimState m) i e -> i -> e -> m ()

--   -- | Swap pixels at given locations.
--   swapA :: (Functor m, PrimMonad m) =>
--            (MA a) (PrimState m) i e -> i -> i -> m ()



-- class ( ColorSpace cs e
--       , Eq (arr (Int, Int) (Pixel cs e))
--       , IArray arr (Int, Int) (Pixel cs e)
--       , VG.Vector (Vector arr) (Pixel cs e)
--       ) =>
--       Array arr cs e where

--   toVectorA :: arr (Int, Int) (Pixel cs e) -> (Vector arr) (Pixel cs e)

--   fromVectorA :: (Int, Int) -> (Vector arr) (Pixel cs e) -> arr (Int, Int) (Pixel cs e)



-- class ( ColorSpace cs e
--       , Eq (arr (Int, Int) (Pixel cs e))
--       , ManifestArray arr (Int, Int) (Pixel cs e)
--       , VG.Vector (Vector arr) (Pixel cs e)
--       ) =>
--       MArray arr cs e


-- data VU ix px = Vector ix (VU.Vector px)

-- data instance Repr VU = VU

-- instance VU.Unbox px => IArray VU (Int, Int) px where
--   type SuperClass VU (Int, Int) px = VU.Unbox px

--   shapeA (Vector sz _) = sz

--   indexA (Vector (_, n) v) !ix = VU.unsafeIndex v (fromIx n ix)

--   makeA (m, n) f =
--     (Vector (m, n) $ VU.generate (m * n) (f . toIx n))



instance ColorSpace cs e => Num (Pixel cs e) where
  (+)         = liftPx2 (+)
  {-# INLINE (+) #-}
  (-)         = liftPx2 (-)
  {-# INLINE (-) #-}
  (*)         = liftPx2 (*)
  {-# INLINE (*) #-}
  abs         = liftPx abs
  {-# INLINE abs #-}
  signum      = liftPx signum
  {-# INLINE signum #-}
  fromInteger = promote . fromInteger
  {-# INLINE fromInteger #-}


instance (ColorSpace cs e, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftPx2 (/)
  {-# INLINE (/) #-}
  recip        = liftPx recip
  {-# INLINE recip #-}
  fromRational = promote . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs e, Floating e) => Floating (Pixel cs e) where
  pi      = promote pi
  {-# INLINE pi #-}
  exp     = liftPx exp
  {-# INLINE exp #-}
  log     = liftPx log
  {-# INLINE log #-}
  sin     = liftPx sin
  {-# INLINE sin #-}
  cos     = liftPx cos
  {-# INLINE cos #-}
  asin    = liftPx asin
  {-# INLINE asin #-}
  atan    = liftPx atan
  {-# INLINE atan #-}
  acos    = liftPx acos
  {-# INLINE acos #-}
  sinh    = liftPx sinh
  {-# INLINE sinh #-}
  cosh    = liftPx cosh
  {-# INLINE cosh #-}
  asinh   = liftPx asinh
  {-# INLINE asinh #-}
  atanh   = liftPx atanh
  {-# INLINE atanh #-}
  acosh   = liftPx acosh
  {-# INLINE acosh #-}

instance (ColorSpace cs e, Bounded e) => Bounded (Pixel cs e) where
  maxBound = promote maxBound
  {-# INLINE maxBound #-}
  minBound = promote minBound
  {-# INLINE minBound #-}

instance (Foldable (Pixel cs), NFData e) => NFData (Pixel cs e) where

  rnf = foldr' deepseq ()
  {-# INLINE rnf #-}
