module Main where

import Text.Lexer.Parser
import Text.Lexer.StateMachine

main :: IO ()
main = do
  let sm = parsePattern "a.?c"
  print sm
  print $ run sm "ac"
