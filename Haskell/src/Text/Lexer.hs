
module Text.Lexer(scan) where

import Data.List
import Data.Maybe
import Control.Arrow

import Text.Lexer.Stream as Stream
import Text.Lexer.Parser
import Text.Lexer.StateMachine

scan :: [(String, String -> Maybe token)] -> String -> (Bool, [token])
scan lexerDef stream = go stream []
  where 
    lexerDef' = map (first parsePattern) lexerDef
    go input tokens = case Stream.uncons input of
       Nothing -> (True, tokens)
       _ -> 
        let 
          lengths = map (\(sm, fun'') -> (run sm input, fun'')) lexerDef'
          maxLength = foldl' (\maxPos (pos, _) -> max maxPos pos) 0 lengths
          (Just (_, fun')) = find (\(pos, _) -> maxLength == pos) lengths
        in
          if maxLength == 0
            then (False, tokens)
            else
              let 
                (curr, rest) = fromJust $ consume maxLength input
              in
                go rest $ tokens ++ maybeToList (fun' curr)
