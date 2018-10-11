
module Text.Lexer.Predicate where

data Predicate = Predicate
  {
    predDescription :: !String,
    predFun :: Char -> Bool
  }

instance Show Predicate where
  show = predDescription

charPredicate :: Char -> Predicate
charPredicate c = Predicate { predDescription = show c, predFun = (== c) }

anyCharPredicate :: Predicate
anyCharPredicate = Predicate { predDescription = ".", predFun = const True }
