
module Text.Yalex.Predicate where

import Data.List

data Predicate c = Predicate
  {
    predDescription :: !String,
    predFun :: c -> Bool
  }

instance Show (Predicate c) where
  show = predDescription


data CharRanges c = CharRanges
  {
    chars :: [c],
    ranges :: [(c, c)]
  }
  deriving Show

instance (Show c, Ord c) => Eq (CharRanges c) where
  (CharRanges c0 r0)  == (CharRanges c1 r1) = sort c0 == sort c1 && sort r0 == sort r1

charPredicate :: (Show c, Ord c) => c -> Predicate c
charPredicate c = Predicate { predDescription = show c, predFun = (== c) }

anyCharPredicate :: Predicate c
anyCharPredicate = Predicate { predDescription = "'.'", predFun = const True }

rangesPredicate :: (Show c, Ord c) => CharRanges c -> Predicate c
rangesPredicate crs@(CharRanges cs rs) = Predicate 
  { 
    predDescription = "Range: " ++ show crs, 
    predFun = \c -> c `elem` cs || any (\(from, to) -> c >= from && c <= to) rs
  }