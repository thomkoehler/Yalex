
module Text.Yalex(scan, scan') where

import Data.List
import Data.Maybe
import Control.Arrow

import Text.Yalex.Stream as Stream
import Text.Yalex.Parser
import Text.Yalex.StateMachine

scan :: [(String, String -> Maybe t)] -> String -> (Bool, [t])
scan lexerDef = scan' lexerDef'
  where 
    lexerDef' = map (first parsePattern) lexerDef
   
scan' :: Stream s c => [(StateMachine c, s -> Maybe t)] -> s -> (Bool, [t])
scan' lexerDef stream = go stream []
  where 
    go input tokens = case Stream.uncons input of
       Nothing -> (True, tokens)
       _ -> 
        let 
          lengths = map (\(sm, fun'') -> (run sm input, fun'')) lexerDef
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
