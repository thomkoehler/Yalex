{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Text.Yalex.StateMachine
import {-@ HTF_TESTS @-} Test.Text.Yalex
import {-@ HTF_TESTS @-} Test.Text.Yalex.PatternParser


main :: IO ()
main = htfMain htf_importedTests