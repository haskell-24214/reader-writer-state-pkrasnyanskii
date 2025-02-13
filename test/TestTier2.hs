module TestTier2 (main) where

import Test.HUnit
import Share (testTree, tryRunTest)

import Control.Monad.Writer (Sum (..), runWriter)

import Tier2.Writer

test_collectAndSumInOrder :: Test
test_collectAndSumInOrder = TestCase $ assertEqual "collect and sum in-order" expected result
  where result = runWriter $ collectAndSumInOrder testTree
        expected = ([1..9], Sum 45)

main :: IO ()
main = tryRunTest $ TestList
  [ TestLabel "test_collectAndSumInOrder" test_collectAndSumInOrder
  ]
