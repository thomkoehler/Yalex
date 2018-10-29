
module Text.Yalex.PatternParser
(
  parsePattern, 
  parseRangePattern
) 
where

import Control.Arrow
import Data.List
import Text.ParserCombinators.Parsec as Parsec

import Text.Yalex.Predicate
import Text.Yalex.StateMachine as SM


metaChars :: String
metaChars = ".*+-?()[]\\"

unescapeChar :: Char -> Char
unescapeChar 'n' = '\n'
unescapeChar 'r' = '\r'
unescapeChar 't' = '\t'
unescapeChar 'f' = '\f'
unescapeChar c = c

charRanges :: Parser (StateMachine Char)
charRanges = do
  _ <- char '['
  cs <- Parsec.many1 (noneOf "]")
  _ <- char ']'
  return $ newStateMachine $ rangesPredicate $ parseRangePattern cs

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
      Text.Yalex.PatternParser.anyChar,
      charRanges,
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

data EscChar 
  = SimpleChar Char
  | EscChar Char
  deriving Eq

toEscChars :: String -> [EscChar]
toEscChars [] = []
toEscChars ('\\' : c  : rest) = EscChar c : toEscChars rest
toEscChars (c : rest) = SimpleChar c : toEscChars rest

fromEscChar :: EscChar -> Char
fromEscChar (SimpleChar c) = c
fromEscChar (EscChar c) = unescapeChar c

parseRangePattern :: String -> CharRanges Char
parseRangePattern patt' = case nextResult of
  (charSet, charRanges', False, Nothing) -> CharRanges {chars = map fromEscChar charSet, ranges = map (fromEscChar *** fromEscChar) charRanges'}
  (charSet, charRanges', False, Just char') -> CharRanges { chars = map fromEscChar (char' : charSet), ranges = map (fromEscChar *** fromEscChar) charRanges'}
  _ -> error "Wrong range pattern encountered"
  where
    nextFun :: ([EscChar], [(EscChar, EscChar)], Bool, Maybe  EscChar) -> EscChar -> ([EscChar], [(EscChar, EscChar)], Bool, Maybe  EscChar) 
    nextFun (_, _, True, Nothing) _ = error "Wrong range pattern encountered"
    nextFun (charSet, charRanges', True, Just lastChar) char' = (charSet, (lastChar, char') : charRanges', False, Nothing)
    nextFun (charSet, charRanges', False, mbLastChar) char' = case (char', mbLastChar) of
      (SimpleChar '-', _) -> (charSet, charRanges', True, mbLastChar)
      (_, Nothing) -> (charSet, charRanges', False, Just char')
      (_, Just lastChar) -> (lastChar : charSet, charRanges', False, Just char')

    nextResult = foldl nextFun ([], [], False, Nothing) $ toEscChars patt'
    