
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Lexer.Stream where

import qualified Data.Text as T


class Stream s c | s -> c where
  uncons :: s -> Maybe (c, s)

instance Stream [x] x where
  uncons [] = Nothing
  uncons (c:cs) = Just (c, cs)

instance Stream T.Text Char where
  uncons = T.uncons
