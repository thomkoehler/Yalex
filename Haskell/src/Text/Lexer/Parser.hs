
module Text.Lexer.Parser(parsePattern) where

import Data.List
import Text.ParserCombinators.Parsec as Parsec

import Text.Lexer.Token
import Text.Lexer.StateMachine

simpleChar :: Parser StateMachine
simpleChar = fmap (newStateMachine . tokenPred . TokenChar) letter

patt :: Parser StateMachine
patt = do
  sms <- Parsec.many simpleChar
  return $ foldl1' (<>) sms

parsePattern :: String -> StateMachine
parsePattern input = case parse patt "" input of
  Left err -> error $ show err
  Right sm -> sm
