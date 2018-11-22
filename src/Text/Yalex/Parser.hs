
module Text.Yalex.Parser where

import Text.Parsec
import Data.Functor.Identity
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

type Parser a = Parsec String () a

languageDef :: GenLanguageDef String () Identity
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames =
         [
            "let",
            "in",
            "Pack",
            "case",
            "of",
            "->"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["+", "-", "*", "/", "<", ">"],
      P.caseSensitive  = True
   }

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser languageDef

identifier :: Parser String
identifier = P.identifier lexer

rule :: Parser (String, String)
rule = undefined