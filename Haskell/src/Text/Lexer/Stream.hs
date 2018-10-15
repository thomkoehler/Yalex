{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Lexer.Stream where

import qualified Data.Text as T


class Stream s c | s -> c where
  uncons :: s -> Maybe (c, s)
  cons :: c -> s -> s
  empty :: s

instance Stream [x] x where
  uncons [] = Nothing
  uncons (c:cs) = Just (c, cs)
  cons c cs = c:cs
  empty = []

instance Stream T.Text Char where
  uncons = T.uncons
  cons = T.cons
  empty = T.empty

consume :: Stream s c => Int -> s -> Maybe (s, s)
consume 0 s = Just (empty, s)
consume length s = case uncons s of
  Nothing -> Nothing
  Just (c, cs) -> case consume (length - 1) cs of
    Nothing -> Nothing
    Just (cs', rest) -> Just (cons c cs', rest)
