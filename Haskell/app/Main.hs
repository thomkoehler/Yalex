module Main where

import Text.Lexer.StateMachine
import Text.Lexer.Token

sma :: StateMachine
sma = createStateMachine (tokenPred (TokenChar 'a'))

smb :: StateMachine
smb = createStateMachine (tokenPred (TokenChar 'b'))

smc :: StateMachine
smc = createStateMachine (tokenPred (TokenChar 'c'))

smAny :: StateMachine
smAny = createStateMachine (tokenPred TokenAnyChar)

sm = sma <> smb <> smc


main :: IO ()
main = do
  print sma
  print $ sma <> smb
  print $ sma <> smb <> smc

  print $ run sm "a123b"
  print $ run sm "aaaabbbb"
  print $ run sm "a1bcdefg"
  print $ run sm "ab"