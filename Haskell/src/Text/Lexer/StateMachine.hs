
module Text.Lexer.StateMachine
(
  StateMachine, 
  Predicate(..), 
  createStateMachine, 
  run
) 
where

import Data.Foldable
import Data.Maybe
import Data.List


data Predicate = Predicate
  {
    predDescription :: String,
    predFun :: Char -> Bool
  }

instance Show Predicate where
  show = predDescription

type State = Int
type TransitionTable = [(State, (State, Predicate))]

data StateMachine = StateMachine
  {
    initialState :: !State,
    acceptingState :: !State,
    transitions :: !TransitionTable
  }
  deriving Show

calcNextStates :: StateMachine -> Char -> [State] -> [State]
calcNextStates stateMachine input = concatMap $ calcNextStates' stateMachine input

calcNextStates' :: StateMachine -> Char -> State -> [State]
calcNextStates' stateMachine input state = 
  map fst . filter (\(_, pred) -> predFun pred input) . map snd . filter (\(entryState, _) -> entryState == state) $ transitions stateMachine

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

createStateMachine :: Predicate -> StateMachine
createStateMachine pred = StateMachine 0 1 [(0, (1, pred))]

instance Semigroup StateMachine where
  st0 <> st1 = 
    let
      initialState0 = initialState st0
      maxState0 = foldl' max 0 $ initialState0 : acceptingState st0 : transitionStates st0
      initialState1 = initialState st1
      acceptingState1 = acceptingState st1
      stateChange1 state = if state == initialState1 then acceptingState1 else state + maxState0
      newTransitions1 = changeTransitionStates stateChange1 $ transitions st1
    in
      StateMachine initialState0 (acceptingState1 + maxState0) $ transitions st0 ++ newTransitions1

transitionStates :: StateMachine -> [Int]
transitionStates (StateMachine is as ts) = filter (\state -> state /= is && state /= as) $ map fst ts

changeTransitionStates :: (State -> State) -> TransitionTable -> TransitionTable
changeTransitionStates change = map (\(st0, (st1, pred)) -> (change st0, (change st1, pred)))