module Main where

import Text.Lexer.StateMachine
import Text.Lexer.Token

sma :: StateMachine
sma = createStateMachine (tokenPred (TokenChar 'a'))

smb :: StateMachine
smb = createStateMachine (tokenPred (TokenChar 'b'))

smAny :: StateMachine
smAny = createStateMachine (tokenPred TokenAnyChar)

sm = sma <> smb <> many smAny <> smb <> sma


main :: IO ()
main = do
  print sm
  print $ run sm "a123b"
  print $ run sm "aaaabbbb"
  print $ run sm "a1bcdefg"
  print $ run sm "ab"
  print $ run sm "abc"
  print $ run sm "abba"
  print $ run sm "abZba"
  print $ run sm "ababbbaabbbaaabbbba"