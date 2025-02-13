module Tier0.Reader (Environment (..), EnvironmentM, formatUserName, formatHost, formatCurrentDir, formatPrompt) where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

formatUserName :: EnvironmentM String
formatUserName = undefined
  
formatHost :: EnvironmentM String
formatHost = undefined

formatCurrentDir :: EnvironmentM String
formatCurrentDir = undefined

formatPrompt :: EnvironmentM String
formatPrompt = undefined
