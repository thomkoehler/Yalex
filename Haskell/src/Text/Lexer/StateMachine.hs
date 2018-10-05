
module Text.Lexer.StateMachine where

type State = Int
type TransitionEntry = (State, Char -> Bool)
type TransitionTable = [(State, (State, Char -> Bool))]

data StateMachine = StateMachine
  {
    transitionTable :: !TransitionTable,
    initialState :: !State,
    acceptingStates :: ![State]
  }

calcNextStates :: StateMachine -> Char -> [State] -> [State]
calcNextStates stateMachine input = concatMap $ calcNextStates' stateMachine input

calcNextStates' :: StateMachine -> Char -> State -> [State]
calcNextStates' stateMachine input state = 
  map fst . filter (\(_, pred) -> pred input) . map snd . filter (\(entryState, _) -> entryState == state) $ transitionTable stateMachine


run :: StateMachine -> String -> Int
run stateMachine text = foldl max 0 positions
  where
    positions = map fst $ snd res
    res = foldl go ([initialState stateMachine], []) $ zip text [1..]
    acceptingStates' = acceptingStates stateMachine
    go :: ([State], [(Int, State)]) -> (Char, Int) -> ([State], [(Int, State)])
    go (currStates, acceptedStates) (char, pos) = 
      if null currStates
        then (currStates, acceptedStates)
        else (nextStates, if null newAcceptedStates then acceptedStates else newAcceptedStates)
      where
        nextStates = calcNextStates stateMachine char currStates
        newAcceptedStates = zip (repeat pos) $ filter (`elem` acceptingStates') nextStates
