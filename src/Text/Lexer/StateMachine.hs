
module Text.Lexer.StateMachine
(
  StateMachine, 
  Predicate(..), 
  newStateMachine, 
  (<|>),
  many1,
  many,
  optional,
  run
) 
where


import Data.Foldable
import Data.List
import Control.Arrow

import Text.Lexer.Predicate
import Text.Lexer.Stream

type State = Int
type TransitionList c = [(State, (State, Predicate c))]

data StateMachine c = StateMachine
  {
    initialState :: !State,
    acceptingState :: !State,
    transitions :: TransitionList c,
    bypasses :: [(State, State)]
  }
  deriving Show

calcNextStates :: StateMachine c -> c -> [State] -> [State]
calcNextStates sm@(StateMachine _ _ _ bps) input states = concatMap (calcNextStates' sm input) $ nub states ++ bypassed
  where
    bypassed = map snd $ filter (\(fromState, _) -> elem fromState states) bps

calcNextStates' :: StateMachine  c -> c -> State -> [State]
calcNextStates' stateMachine input state = map fst . filter (\(_, predicate) -> predFun predicate input) . map snd . filter (\(entryState, _) -> entryState == state) $ transitions stateMachine

run :: (Stream s c) => StateMachine c -> s -> Int
run stateMachine stream = foldl max 0 positions
  where
    acceptingState' = acceptingState stateMachine
    positions = map fst $ snd $ go [initialState stateMachine] [] 1 stream

    go currStates acceptedStates pos currStream = 
      case Text.Lexer.Stream.uncons currStream of
        Nothing -> (currStates, acceptedStates)
        Just (c, cs) -> 
          let
            nextStates = calcNextStates stateMachine c currStates
            newAcceptedStates = zip (repeat pos) $ filter (== acceptingState') nextStates
          in
            if null nextStates
              then (currStates, acceptedStates)
              else go nextStates (if null newAcceptedStates then acceptedStates else newAcceptedStates) (pos + 1) cs

newStateMachine :: Predicate c -> StateMachine c
newStateMachine predicate = StateMachine 0 1 [(0, (1, predicate))] []

instance Semigroup (StateMachine c) where
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

(<|>) :: StateMachine c -> StateMachine c -> StateMachine c
st0 <|> st1 = 
  let
      initialState0 = initialState st0
      acceptingState0 = acceptingState st0
      initialState1 = initialState st1
      acceptingState1 = acceptingState st1
      maxState0 = foldl' max 0 $ initialState0 : acceptingState0 : transitionStates st0
      stateChange1 state
        | state == initialState1 = initialState0
        | state == acceptingState1 = acceptingState0
        | otherwise = state + maxState0

      newTransitions1 = changeTransitionStates stateChange1 $ transitions st1
      newBypasses1 = changeBypassesStates stateChange1 $ bypasses st1
  in
    StateMachine initialState0 acceptingState0 (transitions st0 ++ newTransitions1) (bypasses st0 ++ newBypasses1)

-- regex +
many1 :: StateMachine c -> StateMachine c
many1 sm@(StateMachine is _ ts _) = 
  let
    newTransitions =  map (\(st0, (_, predicate)) -> (st0, (is, predicate))) $ acceptingStateTransitions sm
  in
    sm { transitions = ts ++ newTransitions }

-- regex ?
optional :: StateMachine c -> StateMachine c
optional sm@(StateMachine is as _ bs) = sm { bypasses = (is, as) : bs }

-- regex *
many :: StateMachine c -> StateMachine c
many sm@(StateMachine is as ts _) = sm { acceptingState = is, transitions = map stateChange ts }
  where
    stateChange trans@(st0, (st1, predicate)) = if st1 == as then (st0, (is, predicate)) else trans

transitionStates :: StateMachine c -> [Int]
transitionStates (StateMachine is as ts _) = filter (\state -> state /= is && state /= as) $ map fst ts

changeTransitionStates :: (State -> State) -> TransitionList c -> TransitionList c
changeTransitionStates change = map (\(st0, (st1, predicate)) -> (change st0, (change st1, predicate)))

changeBypassesStates :: (State -> State) -> [(State, State)] -> [(State, State)]
changeBypassesStates change = map $ change *** change

acceptingStateTransitions :: StateMachine c -> TransitionList c
acceptingStateTransitions (StateMachine _ as ts _) = filter (\(_, (state, _)) -> state == as) ts