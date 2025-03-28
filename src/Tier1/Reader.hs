module Tier1.Reader where

import Control.Monad.Reader
import Tier0.Reader

-- Функция cd добавляет новый путь к текущему каталогу
cd :: String -> EnvironmentM a -> EnvironmentM a
cd dir action = local (\env -> env { currentDir = currentDir env ++ "/" ++ dir }) action

-- Функция su переключает пользователя в режим суперпользователя
su :: EnvironmentM a -> EnvironmentM a
su action = local (\env -> env { isSuperUser = True }) action
