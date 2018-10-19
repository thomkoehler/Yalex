
module Text.Lexer.Parser(parsePattern) where

import Data.List
import Text.ParserCombinators.Parsec as Parsec

import Text.Lexer.Predicate
import Text.Lexer.StateMachine as SM


metaChars :: String
metaChars = ".*+-?()[]\\"

unescapeChar :: Char -> Char
unescapeChar 'n' = '\n'
unescapeChar 'r' = '\r'
unescapeChar 't' = '\t'
unescapeChar 'f' = '\f'
unescapeChar c = c

unescapeStr :: String -> String
unescapeStr [] = []
unescapeStr ('\\' : c : rest) = unescapeChar c : unescapeStr rest
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

simplePattern :: Parser (StateMachine Char)
simplePattern = choice
    [
      simpleChar,
      escapeChar,
      Text.Lexer.Parser.anyChar,
      Text.Lexer.Parser.oneOf,
      bracket
    ] 

bracket :: Parser (StateMachine Char)
bracket = do
  _ <- char '('
  ps <- patterns
  _ <- char ')'
  return ps

patt :: Parser (StateMachine Char)
patt = do
  p <- simplePattern
  q <- option id quantifier
  return $ q p;

quantifier :: Parser (StateMachine Char -> StateMachine Char)
quantifier = choice
  [
    char '*' >> return SM.many,
    char '+' >> return SM.many1,
    char '?' >> return SM.optional,
    char '|' >> combinedPattern
  ]


combinedPattern :: Parser (StateMachine Char -> StateMachine Char)
combinedPattern = do
  p <- patt
  return $ \sm -> sm SM.<|> p


patterns :: Parser (StateMachine Char)
patterns = do
  sms <- Parsec.many1 patt
  return $ foldl1' (<>) sms

parsePattern :: String -> StateMachine Char
parsePattern input = case parse patterns "" input of
  Left err -> error $ show err
  Right sm -> sm
