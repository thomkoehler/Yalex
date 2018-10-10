
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Lexer.StateMachine(htf_thisModulesTests) where

import Test.Framework

import Text.Lexer.Parser
import Text.Lexer.StateMachine

prop_SingleChar1 :: Bool
prop_SingleChar1 = stateMachineTest "a" "a" 1

prop_SingleChar2 :: Bool
prop_SingleChar2 = stateMachineTest "a" "b" 0

prop_SingleChar3 :: Bool
prop_SingleChar3 = stateMachineTest "a" "ab" 1

prop_SingleChar4 :: Bool
prop_SingleChar4 = stateMachineTest "a" "ba" 0

prop_Chars1 :: Bool
prop_Chars1 = stateMachineTest "abc" "abc" 3

prop_Chars2 :: Bool
prop_Chars2 = stateMachineTest "abc" "abcdefg" 3

prop_Chars3 :: Bool
prop_Chars3 = stateMachineTest "abc" "bc" 0

prop_AnyChar1 :: Char -> Bool
prop_AnyChar1 c = stateMachineTest "." [c] 1

prop_AnyChar2 :: Char -> Bool
prop_AnyChar2 c = stateMachineTest "ab.cd" ("ab" ++ [c] ++ "cd") 5

prop_AnyChar3 :: Bool
prop_AnyChar3 = stateMachineTest "a." "ba" 0

prop_AnyChar4 :: Bool
prop_AnyChar4 = stateMachineTest "." "" 0

stateMachineTest :: String -> String -> Int -> Bool
stateMachineTest pat input match = run (parsePattern pat) input == match