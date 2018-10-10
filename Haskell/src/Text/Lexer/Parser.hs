
module Text.Lexer.Parser where

import Data.List

import Text.Lexer.Token
import Text.Lexer.StateMachine


parse :: String -> StateMachine
parse input = foldl1' (<>) $  map (newStateMachine . tokenPred . TokenChar) input
