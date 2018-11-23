
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Text.YalexQ(htf_thisModulesTests) where

import Test.Framework

import Text.YalexQ

lexerDef = [yalex| 

    "[ \\r\\n\\t]*" { Nothing }
    "if" {Just TokenIf}
    "then" {Just TokenThen}
    "else" {Just TokenElse}
    "(\\+|\\-)?[0-9]+", {Just . TokenInt . read $ text}
    "[a-zA-Z][a-zA-Z0-9_]*" {Just . TokenId $ text}

|]

prop_Lexer1 :: Bool
prop_Lexer1 = scan lexerDef "" == (True, [])

prop_Lexer2 :: Bool
prop_Lexer2 = scan lexerDef "\r\n  \t" == (True, [])

prop_Lexer3 :: Bool
prop_Lexer3 = scan lexerDef " -123 " == (True, [TokenInt (-123)])

prop_Lexer4 :: Bool
prop_Lexer4 = scan lexerDef "if 1 then 2 else 3" == (True, [TokenIf, TokenInt 1, TokenThen, TokenInt 2, TokenElse, TokenInt 3])

prop_Lexer5 :: Bool
prop_Lexer5 = scan lexerDef "if 1 then ab else xy" == (True, [TokenIf, TokenInt 1, TokenThen, TokenId "ab", TokenElse, TokenId "xy"])