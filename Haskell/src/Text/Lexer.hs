
module Text.Lexer(scan) where

import Text.Lexer.Stream

scan :: (Stream s c) => [(s, s -> token)] -> s -> [token]
scan = undefined