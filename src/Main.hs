{-# LANGUAGE TypeFamilies, ConstraintKinds, BangPatterns, GADTs, DataKinds, FlexibleInstances, UndecidableInstances #-}
module Main where

import Prelude hiding (map, sum)
import Control.Exception

main :: IO ()
main = do print (f False)
          print (f True)
          print (g undefined) `catchE` \_ -> putStrLn "g exception"
          print (h undefined) `catchE` \_ -> putStrLn "h exception"
          print (i undefined) `catchE` \_ -> putStrLn "i exception"
          putStrLn "Done"

f :: Bool -> String
f (view -> Nothing) = "Got Nothing"
f (view -> Just x)  = "Got Just " ++ show x

g :: Bool -> String
g (view -> x) = "g Got something"

h :: Bool -> String
h (view -> !x) = "h Got something"

i :: Int -> String
i (view' -> (!x, y)) = "i Got something "

view :: Bool -> Maybe Int
view False = Nothing
view True = Just 5

view' :: Num a => a -> (a, a)
view' x = (x, x+1)

catchE :: IO a -> (ErrorCall -> IO a) -> IO a
catchE = catch
