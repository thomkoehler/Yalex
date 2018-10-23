
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Yalex.StateMachine(htf_thisModulesTests) where

import Test.Framework

import Text.Yalex.PatternParser
import Text.Yalex.StateMachine

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

prop_Optional1 :: Bool
prop_Optional1 = stateMachineTest ".?" "a" 1

prop_Optional2 :: Bool
prop_Optional2 = stateMachineTest ".?" "" 0

prop_Optional3 :: Bool
prop_Optional3 = stateMachineTest "a.?c" "abc" 3

prop_Optional4 :: Bool
prop_Optional4 = stateMachineTest "a.?c" "ac" 2

prop_Optional5 :: Bool
prop_Optional5 = stateMachineTest "aa?c" "ac" 2

prop_Optional6 :: Bool
prop_Optional6 = stateMachineTest "aa?c" "aac" 3

prop_Many1 :: Bool
prop_Many1 = stateMachineTest "a.*c" "ac" 2

prop_Many2 :: Bool
prop_Many2 = stateMachineTest "a.*c" "abc" 3

prop_Many3 :: Bool
prop_Many3 = stateMachineTest "a.*c" "aaaaabcccc" 10

prop_Many11 :: Bool
prop_Many11 = stateMachineTest "a+" "a" 1

prop_Many12 :: Bool
prop_Many12 = stateMachineTest "a+b" "b" 0

prop_Many13 :: Bool
prop_Many13 = stateMachineTest "ab+c" "abc" 3

prop_Many14 :: Bool
prop_Many14 = stateMachineTest "ab+c" "abbbbc" 6

prop_Escape1 :: Bool
prop_Escape1 = stateMachineTest "\n" "\n" 1

prop_Escape2 :: Bool
prop_Escape2 = stateMachineTest "\n*" "\n\n\n" 3

prop_oneOf1 :: Bool
prop_oneOf1 = stateMachineTest "a[1234567890]*b" "a321b" 5

prop_oneOf2 :: Bool
prop_oneOf2 = stateMachineTest "a[1234567890]*b" "321b" 0

prop_or0 :: Bool
prop_or0 = stateMachineTest "a|b" "a" 1

prop_or1 :: Bool
prop_or1 = stateMachineTest "a|b" "b" 1

prop_or2 :: Bool
prop_or2 = stateMachineTest "a|b" "c" 0

prop_or3 :: Bool
prop_or3 = stateMachineTest "(ab)|(cd)" "ab" 2

prop_or4 :: Bool
prop_or4 = stateMachineTest "(ab)|(cd)" "cd" 2

prop_or5 :: Bool
prop_or5 = stateMachineTest "(ab)|(cd)" "ad" 0

stateMachineTest :: String -> String -> Int -> Bool
stateMachineTest pat input match = run (parsePattern pat) input == match