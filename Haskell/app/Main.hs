module Main where

import Text.Lexer.Parser
import Text.Lexer.StateMachine

main :: IO ()
main = do
  let sm = parse "abba"
  print sm
  print $ run sm "a123b"
  print $ run sm "aaaabbbb"
  print $ run sm "a1bcdefg"
  print $ run sm "ab"
  print $ run sm "abc"
  print $ run sm "abba"
  print $ run sm "abZba"
  print $ run sm "ababbbaabbbaaabbbba"