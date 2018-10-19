
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Lexer(htf_thisModulesTests) where

import Test.Framework

import Text.Lexer

data Token 
  = TokenIf
  | TokenThen
  | TokenElse
  | TokenInt Integer
  deriving Eq

lexerDef :: [(String, String -> Maybe Token)]
lexerDef =
  [
    ("[ \\r\\n\\t]*", const Nothing),
    ("if", const (Just TokenIf)),
    ("then", const (Just TokenThen)),
    ("else", const (Just TokenElse)),
    ("(\\+|\\-)?[0123456789]+", Just . TokenInt . read)
  ]

prop_Lexer1 :: Bool
prop_Lexer1 = scan lexerDef "" == (True, [])

prop_Lexer2 :: Bool
prop_Lexer2 = scan lexerDef "\r\n  \t" == (True, [])

prop_Lexer3 :: Bool
prop_Lexer3 = scan lexerDef " -123 " == (True, [TokenInt (-123)])

prop_Lexer4 :: Bool
prop_Lexer4 = scan lexerDef "if 1 then 2 else 3" == (True, [TokenIf, TokenInt 1, TokenThen, TokenInt 2, TokenElse, TokenInt 3])