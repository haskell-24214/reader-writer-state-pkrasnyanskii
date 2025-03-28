module Tier2.Writer where

import Control.Monad.Writer
import Data.Monoid (Sum(..))
import Tier0.Writer

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf x) = do
  tell (Sum x)
  return [x]
collectAndSumInOrder (Branch left x right) = do
  leftList <- collectAndSumInOrder left
  tell (Sum x)
  rightList <- collectAndSumInOrder right
  return (leftList ++ [x] ++ rightList)
