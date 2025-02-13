module Share (testData, testTree, tryRunTest) where

import Test.HUnit (Counts (..), Test, runTestTT)

import Control.Monad (when)

import Tier0.Reader (Environment (..))
import Tier0.Writer (Tree (..))

testData :: Environment
testData = Environment
  { username = "user"
  , isSuperUser = False
  , host = "localhost"
  , currentDir = "~"
  }

testTree :: Tree Int
testTree = Branch (Branch (Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5))) 6 (Leaf 7)) 8 (Leaf 9)

tryRunTest :: Test -> IO ()
tryRunTest test = do
  counts <- runTestTT test
  when ((errors counts + failures counts) > 0) (error "test failed")
