
module Text.Lexer.Parser(parsePattern) where

import Data.List
import Text.ParserCombinators.Parsec as Parsec

import Text.Lexer.Token
import Text.Lexer.StateMachine

simpleChar :: Parser StateMachine
simpleChar = fmap (newStateMachine . tokenPred . TokenChar) letter

anyChar :: Parser StateMachine
anyChar = do
  char '.'
  return $ newStateMachine $ tokenPred TokenAnyChar

patt :: Parser StateMachine
patt = do
  p <- choice
    [
      Text.Lexer.Parser.anyChar,
      simpleChar
    ] 

  q <- quantifier
  return $ q p;

quantifier :: Parser (StateMachine -> StateMachine)
quantifier = return id

patterns :: Parser StateMachine
patterns = do
  sms <- Parsec.many1 patt
  return $ foldl1' (<>) sms

parsePattern :: String -> StateMachine
parsePattern input = case parse patterns "" input of
  Left err -> error $ show err
  Right sm -> sm
