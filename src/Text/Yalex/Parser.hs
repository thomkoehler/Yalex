
module Text.Yalex.Parser where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

languageDef :: GenLanguageDef String () Identity
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames = [],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = [],
      P.caseSensitive  = True
   }

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser languageDef

identifier :: Parser String
identifier = P.identifier lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

rule :: Parser (String, String)
rule = do
   patt <- stringLiteral
   _ <- char '{'
   src <- manyTill anyChar (try (char '}'))
   return (patt, src)
