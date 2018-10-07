module Main where

import Text.Lexer.StateMachine

isA = (== 'a')
isB = (== 'b')
anyChar = const True

sm = StateMachine 
  {
    transitionTable = [(0, (1, isA)), (1, (2, anyChar)), (1, (1, anyChar)), (2, (3, isB))],
    initialState = 0,
    acceptingStates = [3]
  }

main :: IO ()
main = do
  print $ run sm "a123b"
  print $ run sm "aaaabbbb"
  print $ run sm "a1bcdefg"
  print $ run sm "ab"