{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Graphics.Image.Utils
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Utils
  ( loop
  , loopM_
  , (.:)
  , (.:!)
  , swapIx
  , fromIx
  , toIx
  , checkDims
  ) where

-- | Boob operator: @((.).(.))@
(.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.:) f g = \ x y -> f (g x y)
{-# INLINE (.:) #-}


-- | Strict version of boob operator: @((.).(.))@
(.:!) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.:!) f g = \ !x !y -> let !z = g x y in f z
{-# INLINE (.:!) #-}


-- | Very efficient loop
loop :: t -> (t -> Bool) -> (t -> t) -> a -> (t -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> acc
      True  -> go (increment step) (f step acc)
{-# INLINE loop #-}

-- | Very efficient monadic loop
loopM_ :: Monad m => t -> (t -> Bool) -> (t -> t) -> (t -> m a) -> m ()
loopM_ !init' condition increment f = go init' where
  go !step =
    case condition step of
      False -> return ()
      True  -> f step >> go (increment step)
{-# INLINE loopM_ #-}


swapIx :: (a, b) -> (b, a)
swapIx !(i, j) = (j, i)
{-# INLINE swapIx #-}


fromIx :: Int -- ^ @n@ columns
       -> (Int, Int) -- ^ @(i, j)@ row, column index
       -> Int -- ^ Flat vector index
fromIx !n !(i, j) = n * i + j
{-# INLINE fromIx #-}

toIx :: Int -- ^ @n@ columns
     -> Int -- ^ Flat vector index
     -> (Int, Int) -- ^ @(i, j)@ row, column index
toIx !n !k = divMod k n
{-# INLINE toIx #-}



checkDims :: String -> (Int, Int) -> (Int, Int)
checkDims err !sz@(m, n)
  | m <= 0 || n <= 0 =
    error $
    show err ++ ": dimensions are expected to be positive: " ++ show sz
  | otherwise = sz
{-# INLINE checkDims #-}

