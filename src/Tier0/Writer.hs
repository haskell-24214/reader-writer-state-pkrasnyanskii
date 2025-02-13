module Tier0.Writer (Tree (..), sumAndTraceInOrder) where

import Control.Monad.Writer

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

sumAndTraceInOrder :: Num a => Tree a -> Writer [a] a
sumAndTraceInOrder = undefined
