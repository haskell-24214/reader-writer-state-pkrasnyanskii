import System.Environment

import qualified TestTier0 as T0 (main)
import qualified TestTier1 as T1 (main)
import qualified TestTier2 as T2 (main)

main :: IO ()
main = do
   getArgs >>= \args -> case args of
    ("tier0":_) -> T0.main
    ("tier1":_) -> T1.main
    ("tier2":_) -> T2.main
    _ -> putStrLn "please specify test suite (tier0/tier1/tier2)"
