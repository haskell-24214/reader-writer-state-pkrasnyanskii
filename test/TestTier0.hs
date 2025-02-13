module TestTier0 (main) where

import Test.HUnit
import Share (testData, testTree, tryRunTest)

import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriter)

import Tier0.Reader
import Tier0.Writer

test_formatUserName1 :: Test
test_formatUserName1 = TestCase $ assertEqual "just user name" expected result
  where result = runReader formatUserName testData
        expected = "user"

test_formatUserName2 :: Test
test_formatUserName2 = TestCase $ assertEqual "root user name" expected result
  where result = runReader formatUserName (testData { isSuperUser = True })
        expected = "root"

test_formatHost :: Test
test_formatHost = TestCase $ assertEqual "host name" expected result
  where result = runReader formatHost testData
        expected = "localhost"

test_formatCurrentDir :: Test
test_formatCurrentDir = TestCase $ assertEqual "current dir" expected result
  where result = runReader formatCurrentDir testData
        expected = "~"

test_formatPrompt :: Test
test_formatPrompt = TestCase $ assertEqual "prompt" expected result
  where result = runReader formatPrompt testData
        expected = "user@localhost:~$"

test_sumAndTraceInOrder :: Test
test_sumAndTraceInOrder = TestCase $ assertEqual "sum and trace in-order" expected result
  where result = runWriter $ sumAndTraceInOrder testTree
        expected = (45, [1..9])

main :: IO ()
main = tryRunTest $ TestList
  [ TestLabel "test_formatUserName1" test_formatUserName1
  , TestLabel "test_formatUserName2" test_formatUserName2
  , TestLabel "test_formatHost" test_formatHost
  , TestLabel "test_formatCurrentDir" test_formatCurrentDir
  , TestLabel "test_formatPrompt" test_formatPrompt
  , TestLabel "test_sumAndTraceInOrder" test_sumAndTraceInOrder
  ]
