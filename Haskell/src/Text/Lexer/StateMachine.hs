
module Text.Lexer.StateMachine where

import Data.Foldable
import Data.Maybe

type State = Int
type TransitionTable = [(State, (State, Char -> Bool))]

data StateMachine = StateMachine
  {
    transitions :: !TransitionTable,
    initialState :: !State,
    acceptingState :: !State
  }

calcNextStates :: StateMachine -> Char -> [State] -> [State]
calcNextStates stateMachine input = concatMap $ calcNextStates' stateMachine input

calcNextStates' :: StateMachine -> Char -> State -> [State]
calcNextStates' stateMachine input state = 
  map fst . filter (\(_, pred) -> pred input) . map snd . filter (\(entryState, _) -> entryState == state) $ transitions stateMachine

run :: StateMachine -> String -> Int
run stateMachine text = foldl max 0 positions
  where
    acceptingState' = acceptingState stateMachine
    positions = map fst $ snd $ go [initialState stateMachine] [] 1 text

    go :: [State] -> [(Int, State)] -> Int -> String -> ([State], [(Int, State)])
    go currStates acceptedStates pos currText = 
      case currText of
        [] ->  (currStates, acceptedStates)
        (c:cs) -> 
          let
            nextStates = calcNextStates stateMachine c currStates
            newAcceptedStates = zip (repeat pos) $ filter (== acceptingState') nextStates
          in
            if null nextStates
              then (currStates, acceptedStates)
              else go nextStates (if null newAcceptedStates then acceptedStates else newAcceptedStates) (pos + 1) cs


transitionStates :: StateMachine -> [Int]
transitionStates (StateMachine ts is as) = filter (\state -> state /= is && state /= as) $ map fst ts

changeStates :: [(State, State)] -> StateMachine -> StateMachine
changeStates stateChanges stateMachine@(StateMachine ts is as) = 
  let
    lookupState oldState = fromJust $ lookup oldState stateChanges
    newTransitions = map (\(st0, (st1, pred)) -> (lookupState st0, (lookupState st1, pred))) ts
  in
    StateMachine newTransitions (lookupState is) (lookupState as)


instance Semigroup StateMachine where
  st0 <> st1 = undefined
