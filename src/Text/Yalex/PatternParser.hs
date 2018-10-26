
module Text.Yalex.PatternParser(parsePattern) where

import Data.List
import Data.Maybe
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
      Text.Yalex.PatternParser.anyChar,
      Text.Yalex.PatternParser.oneOf,
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

fromEscChars :: [EscChar] -> String
fromEscChars = map conv
  where
    conv (SimpleChar c) = c
    conv (EscChar c) = unescapeChar c

parseRangePattern :: String -> (String, [(Char, Char)])
parseRangePattern patt = undefined
  where
    nextFun :: ([EscChar], [(EscChar, EscChar)], Bool, Maybe  EscChar) -> EscChar -> ([EscChar], [(EscChar, EscChar)], Bool, Maybe  EscChar) 
    nextFun (charSet, charRanges, inRange, mbLastChar) char = undefined
    nextFun (_, _, True, Nothing) char = error "Wrong range pattern encountered"
    nextFun (charSet, charRanges, True, Just lastChar) char = (charSet, (lastChar, char) : charRanges, False, Nothing)
    nextFun (charSet, charRanges, False, mbLastChar) char = case (char, mbLastChar) of
      (SimpleChar '-', _) -> (charSet, charRanges, True, mbLastChar)
      (_, Nothing) -> (charSet, charRanges, False, Just char)
      _ -> (char : charSet, charRanges, False, mbLastChar)

    nextResult = foldl nextFun ([], [], False, Nothing) $ toEscChars patt
      
      