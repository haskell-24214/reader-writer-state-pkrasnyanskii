module TestTier1 (main) where

import Test.HUnit
import Share (testData, tryRunTest)

import Control.Monad.Reader (runReader)

import Tier0.Reader
import Tier1.Reader

test_cd :: Test
test_cd = TestCase $ assertEqual "cd" expected result
  where result = runReader (cd "Documents" formatPrompt) testData
        expected = "user@localhost:~/Documents$"

test_su :: Test
test_su = TestCase $ assertEqual "su" expected result
  where result = runReader (su formatPrompt) testData
        expected = "root@localhost:~$"

test_suCdCd :: Test
test_suCdCd = TestCase $ assertEqual "su && cd && cd" expected result
  where result = runReader (su (cd "Documents" (cd "Haskell" formatPrompt))) testData
        expected = "root@localhost:~/Documents/Haskell$"

main :: IO ()
main = tryRunTest $ TestList
  [ TestLabel "test_cd" test_cd
  , TestLabel "test_su" test_su
  , TestLabel "test_suCdCd" test_suCdCd
  ]
