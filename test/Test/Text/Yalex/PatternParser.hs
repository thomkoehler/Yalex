{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.Yalex.PatternParser(htf_thisModulesTests) where

import Test.Framework

import Text.Yalex.PatternParser
import Text.Yalex.Predicate

prop_RangePattern0 :: Bool
prop_RangePattern0 = parseRangePattern "" == CharRanges "" []

prop_RangePattern1 :: Bool
prop_RangePattern1 = parseRangePattern "abc" == CharRanges "abc" []

prop_RangePattern2 :: Bool
prop_RangePattern2 = parseRangePattern "a-b" == CharRanges "" [('a', 'b')]

prop_RangePattern3 :: Bool
prop_RangePattern3 = parseRangePattern "abcy-z" == CharRanges  "abc" [('y', 'z')]

prop_RangePattern4 :: Bool
prop_RangePattern4 = parseRangePattern "abcy-zdefg" == CharRanges  "abcdefg" [('y', 'z')]

prop_RangePattern5 :: Bool
prop_RangePattern5 = parseRangePattern "v-wx-y" == CharRanges  "" [('v', 'w'), ('x', 'y')]

prop_RangePattern6 :: Bool
prop_RangePattern6 = parseRangePattern "abcv-wx-y" == CharRanges  "abc" [('v', 'w'), ('x', 'y')]

prop_RangePattern7 :: Bool
prop_RangePattern7 = parseRangePattern "v-wx-yabc" == CharRanges  "abc" [('v', 'w'), ('x', 'y')]

prop_RangePattern8 :: Bool
prop_RangePattern8 = parseRangePattern "v-wabcx-y" == CharRanges  "abc" [('v', 'w'), ('x', 'y')]

prop_RangePattern9 :: Bool
prop_RangePattern9 = parseRangePattern "abcv-wdefx-yghi" == CharRanges  "abcdefghi" [('v', 'w'), ('x', 'y')]
