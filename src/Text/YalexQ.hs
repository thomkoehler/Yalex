module Text.YalexQ(yalex) where

import Language.Haskell.TH.Quote
import Text.Yalex.Parser

yalex :: QuasiQuoter
yalex = QuasiQuoter
  {
    quoteExp = quoteYalexExp,
    quotePat = error "Cannot use yalex as a pattern",
    quoteType = error "Cannot use yalex as a type",
    quoteDec = error "Cannot use yalex as a dec"
  }

quoteYalexExp txt = do
  exp <- parseExp txt
  dataToExpQ (const Nothing) exp
