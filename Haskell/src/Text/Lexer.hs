
module Text.Lexer(scan) where

import Data.List
import Data.Maybe

import Text.Lexer.Stream as Stream
import Text.Lexer.Parser
import Text.Lexer.StateMachine

scan :: [(String, String -> Maybe token)] -> token -> String -> [token]
scan lexerDef eofToken stream = go stream []
  where 
    lexerDef' = map (\(s, fun) -> (parsePattern s, fun)) lexerDef
    go input tokens = case Stream.uncons input of
       Nothing -> tokens ++ [eofToken]
       _ -> 
        let 
          lengths = map (\(sm, fun) -> (run sm input, fun)) lexerDef'
          maxLength = foldl' (\maxPos (pos, _) -> max maxPos pos) 0 lengths
          (Just (_, fun)) = find (\(pos, fun) -> maxLength == pos) lengths
        in
          if maxLength == 0
            then tokens
            else
              let 
                (curr, rest) = fromJust $ consume maxLength input
              in
                go rest $ tokens ++ maybeToList (fun curr)
