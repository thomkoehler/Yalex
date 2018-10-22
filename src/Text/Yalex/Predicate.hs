
module Text.Yalex.Predicate where

data Predicate c = Predicate
  {
    predDescription :: !String,
    predFun :: c -> Bool
  }

instance Show (Predicate c) where
  show = predDescription

charPredicate :: (Show c, Ord c) => c -> Predicate c
charPredicate c = Predicate { predDescription = show c, predFun = (== c) }

anyCharPredicate :: Predicate c
anyCharPredicate = Predicate { predDescription = "'.'", predFun = const True }

rangesPredicate :: (Show c, Ord c) => [(c, c)] -> Predicate c
rangesPredicate ranges = Predicate 
  { 
    predDescription = "Range: " ++ show ranges, 
    predFun = \c -> any (\(from, to) -> c >= from && c <= to) ranges
  }

oneOfPredicate ::  (Show c, Ord c) => [c] -> Predicate c
oneOfPredicate chars = Predicate
  { 
    predDescription = "One of: " ++ show chars, 
    predFun = flip elem chars
  }