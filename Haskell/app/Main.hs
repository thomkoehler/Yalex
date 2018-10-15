module Main where

import Text.Lexer.Parser
import Text.Lexer.StateMachine
import Text.Lexer

data Token 
  = TokenIf
  | TokenThen
  | TokenElse
  | TokenInt Integer
  deriving (Eq, Show)

lexerDef =
  [
    ("[\\r\\n\\t]*", const Nothing),
    ("if", const (Just TokenIf)),
    ("then", const (Just TokenThen)),
    ("else", const (Just TokenElse)),
    ("[0123456789]+", Just . TokenInt . read)
  ]


main :: IO ()
main = do
  let sm = parsePattern "a[1234567890]*b"
  print sm
  print $ run sm "321b"
  print $ scan lexerDef "if 123"
