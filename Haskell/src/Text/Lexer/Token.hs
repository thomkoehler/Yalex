
module Text.Lexer.Token where

import Text.Lexer.StateMachine

data Token
  = TokenChar Char
  | TokenAnyChar

tokenPred :: Token -> Predicate
tokenPred (TokenChar c) = Predicate { predDescription = show c, predFun = (== c) }
tokenPred TokenAnyChar = Predicate { predDescription = "*", predFun = const True }