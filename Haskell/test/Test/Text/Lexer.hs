
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Lexer(htf_thisModulesTests) where

import Test.Framework

import Text.Lexer.Parser
import Text.Lexer


data Token 
  = Token
  | TokenEof
  deriving Eq

lexerDef =
  [
    (":*", \s -> Nothing)
  ]

prop_Lexer :: Bool
prop_Lexer = scan lexerDef TokenEof "" == [TokenEof]