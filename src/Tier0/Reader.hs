module Tier0.Reader where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

-- Возвращает имя пользователя или "root" для суперпользователя
formatUserName :: EnvironmentM String
formatUserName = do
  env <- ask
  return $ if isSuperUser env then "root" else username env

-- Возвращает имя хоста
formatHost :: EnvironmentM String
formatHost = asks host

-- Возвращает текущую папку
formatCurrentDir :: EnvironmentM String
formatCurrentDir = asks currentDir

-- Формирует приглашение командной строки
formatPrompt :: EnvironmentM String
formatPrompt = do
  user <- formatUserName
  hostname <- formatHost
  cwd <- formatCurrentDir
  return $ user ++ "@" ++ hostname ++ ":" ++ cwd ++ "$"
