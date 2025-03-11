module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = do
  s <- get
  let newAcc = ax s + bx s
  put s { acc = newAcc, blink = False }
  return newAcc

minus :: Calculation
minus = do
  s <- get
  let newAcc = ax s - bx s
  put s { acc = newAcc, blink = False }
  return newAcc

productS :: Calculation
productS = do
  s <- get
  let newAcc = ax s * bx s
  put s { acc = newAcc, blink = False }
  return newAcc

divide :: Calculation
divide = do
  s <- get
  if bx s == 0
    then do
      put emptyRegisters
      return 0
    else do
      let newAcc = ax s `div` bx s
      put s { acc = newAcc, blink = False }
      return newAcc

swap :: Calculation
swap = do
  s <- get
  put s { ax = bx s, bx = ax s }
  return 0

blinkS :: Calculation
blinkS = do
  s <- get
  put s { blink = not (blink s) }
  return 0

accS :: Calculation
accS = do
  s <- get
  if blink s
    then put s { bx = acc s, blink = not (blink s) }
    else put s { ax = acc s, blink = not (blink s) }
  return 0

number :: Int -> Calculation
number x = do
  s <- get
  if blink s
    then put s { bx = x, blink = not (blink s) }
    else put s { ax = x, blink = not (blink s) }
  return x

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    "/"    -> divide
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters
