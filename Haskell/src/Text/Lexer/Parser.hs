
module Text.Lexer.Parser(parsePattern) where

import Data.List
import Text.ParserCombinators.Parsec as Parsec

import Text.Lexer.Predicate
import Text.Lexer.StateMachine as SM


metaChars :: String
metaChars = "*+?()"

simpleChar :: Parser StateMachine
simpleChar = fmap (newStateMachine . charPredicate) (noneOf metaChars)

anyChar :: Parser StateMachine
anyChar = char '.' >> return (newStateMachine anyCharPredicate)

patt :: Parser StateMachine
patt = do
  p <- choice
    [
      Text.Lexer.Parser.anyChar,
      simpleChar
    ] 

  q <- option id quantifier
  return $ q p;

quantifier :: Parser (StateMachine -> StateMachine)
quantifier = choice
  [
    char '*' >> return SM.many,
    char '+' >> return SM.many1,
    char '?' >> return SM.optional
  ]

patterns :: Parser StateMachine
patterns = do
  sms <- Parsec.many1 patt
  return $ foldl1' (<>) sms

parsePattern :: String -> StateMachine
parsePattern input = case parse patterns "" input of
  Left err -> error $ show err
  Right sm -> sm
