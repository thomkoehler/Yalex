
module Text.Lexer.StateMachine
(
  StateMachine, 
  Predicate(..), 
  newStateMachine, 
  many1,
  many,
  run
) 
where

import Data.Foldable
import Data.Maybe
import Data.List
import Control.Arrow

data Predicate = Predicate
  {
    predDescription :: !String,
    predFun :: Char -> Bool
  }

instance Show Predicate where
  show = predDescription

type State = Int
type TransitionList = [(State, (State, Predicate))]

data StateMachine = StateMachine
  {
    initialState :: !State,
    acceptingState :: !State,
    transitions :: !TransitionList,
    bypasses :: [(State, State)]
  }
  deriving Show

calcNextStates :: StateMachine -> Char -> [State] -> [State]
calcNextStates stateMachine input = concatMap $ calcNextStates' stateMachine input

calcNextStates' :: StateMachine -> Char -> State -> [State]
calcNextStates' stateMachine input state = nub $ nextStates ++ bypassedStates
  where
    nextStates = map fst . filter (\(_, pred) -> predFun pred input) . map snd . filter (\(entryState, _) -> entryState == state) $ transitions stateMachine
    bypassedStates = map snd . filter (\(st, _) -> st == state) $ bypasses stateMachine

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

newStateMachine :: Predicate -> StateMachine
newStateMachine pred = StateMachine 0 1 [(0, (1, pred))] []

instance Semigroup StateMachine where
  st0 <> st1 = 
    let
      initialState0 = initialState st0
      acceptingState0 = acceptingState st0
      maxState0 = foldl' max 0 $ initialState0 : acceptingState0 : transitionStates st0
      stateChange1 state = if state == initialState st1 then acceptingState0 else state + maxState0
      newTransitions1 = changeTransitionStates stateChange1 $ transitions st1
      newBypasses1 = changeBypassesStates stateChange1 $ bypasses st1
    in
      StateMachine initialState0 (acceptingState st1 + maxState0) (transitions st0 ++ newTransitions1) (bypasses st0 ++ newBypasses1)

-- regex +
many1 :: StateMachine -> StateMachine
many1 sm@(StateMachine is as ts _) = 
  let
    newTransitions =  map (\(st0, (st1, pred)) -> (st0, (is, pred))) $ acceptingStateTransitions sm
  in
    sm { transitions = ts ++ newTransitions }

-- regex ?
optional :: StateMachine -> StateMachine
optional sm@(StateMachine is as _ bs) = sm { bypasses = (is, as) : bs }

-- regex *
many :: StateMachine -> StateMachine
many sm@(StateMachine is as ts _) = sm { acceptingState = is, transitions = map stateChange ts }
  where
    stateChange trans@(st0, (st1, pred)) = if st1 == as then (st0, (is, pred)) else trans

transitionStates :: StateMachine -> [Int]
transitionStates (StateMachine is as ts _) = filter (\state -> state /= is && state /= as) $ map fst ts

changeTransitionStates :: (State -> State) -> TransitionList -> TransitionList
changeTransitionStates change = map (\(st0, (st1, pred)) -> (change st0, (change st1, pred)))

changeBypassesStates :: (State -> State) -> [(State, State)] -> [(State, State)]
changeBypassesStates change = map $ change *** change

acceptingStateTransitions :: StateMachine -> TransitionList
acceptingStateTransitions (StateMachine _ as ts _) = filter (\(_, (state, _)) -> state == as) ts