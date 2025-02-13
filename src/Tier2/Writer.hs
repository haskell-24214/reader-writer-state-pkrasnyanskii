module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder = undefined
