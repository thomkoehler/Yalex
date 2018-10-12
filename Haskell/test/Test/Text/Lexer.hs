
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Lexer(htf_thisModulesTests) where

import Test.Framework

import Text.Lexer.Parser
import Text.Lexer


data Token 
  = Token
  deriving Eq

prop_Lexer :: Bool
prop_Lexer = null $ scan [(".*", const Token)] ""