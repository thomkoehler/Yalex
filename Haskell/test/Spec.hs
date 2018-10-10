{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Text.Lexer.StateMachine


main :: IO ()
main = htfMain htf_importedTests