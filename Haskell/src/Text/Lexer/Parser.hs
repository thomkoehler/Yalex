
module Text.Lexer.Parser(parsePattern) where

import Data.List
import Text.ParserCombinators.Parsec as Parsec


import Text.Lexer.Predicate
import Text.Lexer.StateMachine as SM


metaChars :: String
metaChars = ".*+?()[]\\"

unescapeChar :: Char -> Char
unescapeChar 'n' = '\n'
unescapeChar 'r' = '\r'
unescapeChar 't' = '\t'
unescapeChar 'f' = '\f'
unescapeChar c = c

unescapeStr :: String -> String
unescapeStr [] = []
unescapeStr ('\\' : c : rest) = unescapeChar c : rest
unescapeStr (c:cs) = c : unescapeStr cs

oneOf :: Parser (StateMachine Char)
oneOf = do
  _ <- char '['
  chars <- Parsec.many1 (noneOf "]")
  _ <- char ']'
  return $ newStateMachine $ oneOfPredicate $ unescapeStr chars

escapeChar :: Parser (StateMachine Char)
escapeChar = char '\\' >> fmap (newStateMachine . charPredicate . unescapeChar) Parsec.anyChar 

simpleChar :: Parser (StateMachine Char)
simpleChar = fmap (newStateMachine . charPredicate) (noneOf metaChars)

anyChar :: Parser (StateMachine Char)
anyChar = char '.' >> return (newStateMachine anyCharPredicate)

patt :: Parser (StateMachine Char)
patt = do
  p <- choice
    [
      simpleChar,
      escapeChar,
      Text.Lexer.Parser.anyChar,
      Text.Lexer.Parser.oneOf
    ] 

  q <- option id quantifier
  return $ q p;

quantifier :: Parser (StateMachine c -> StateMachine c)
quantifier = choice
  [
    char '*' >> return SM.many,
    char '+' >> return SM.many1,
    char '?' >> return SM.optional
  ]

patterns :: Parser (StateMachine Char)
patterns = do
  sms <- Parsec.many1 patt
  return $ foldl1' (<>) sms

parsePattern :: String -> StateMachine Char
parsePattern input = case parse patterns "" input of
  Left err -> error $ show err
  Right sm -> sm
