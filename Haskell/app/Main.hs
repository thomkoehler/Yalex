module Main where

import Text.Lexer.Parser
import Text.Lexer.StateMachine

main :: IO ()
main = do
  let sm = parsePattern "a[1234567890]*b"
  print sm
  print $ run sm "321b"
