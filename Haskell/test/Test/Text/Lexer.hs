
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Lexer(htf_thisModulesTests) where

import Test.Framework

import Text.Lexer

data Token 
  = TokenIf
  | TokenThen
  | TokenElse
  | TokenInt Integer
  | TokenEof
  deriving Eq

lexerDef =
  [
    ("[\\r\\n\\t]*", const Nothing),
    ("if", const (Just TokenIf)),
    ("then", const (Just TokenThen)),
    ("else", const (Just TokenElse)),
    ("[0123456789]+", Just . TokenInt . read)
  ]

prop_Lexer1 :: Bool
prop_Lexer1 = scan lexerDef TokenEof "" == [TokenEof]

prop_Lexer2 :: Bool
prop_Lexer2 = scan lexerDef TokenEof " 123 " == [TokenInt 123, TokenEof]

prop_Lexer3 :: Bool
prop_Lexer3 = scan lexerDef TokenEof "if 1 then 2 else 3" == [TokenIf, TokenInt 1, TokenThen, TokenInt 2, TokenElse, TokenInt 3, TokenEof]