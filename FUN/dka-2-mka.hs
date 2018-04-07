-- dka-2-mka, xjochl00, Jakub Jochlík

import System.Environment
import System.IO
import Data.List
import Data.Char
import Data.Ord
import Data.Function
import Data.List.Split
import Control.Monad
import qualified Data.Set as Set


-- naming stuff
type State = String
type Symbol = String
type Alphabet = Set.Set Symbol

-- automat transition rules
data Rule = Rule {
    currentState :: State,
    inputSymbol :: Symbol,
    newState :: State
} deriving (Eq, Ord)     -- to be able to use nub

-- representation of KA
data Automat = Automat {
    states :: Set.Set State,
    startingState :: State,
    endStates :: Set.Set State,
    alphabet :: Alphabet,
    rules :: [Rule]
}

-- for debugging
instance Show Automat where
    show (Automat states startingState endStates alphabet rules) =  
        "states: " ++ show states ++ "\n " ++
        "startingState: " ++ show startingState ++ "\n " ++
        "endingStates: " ++ show endStates ++ "\n " ++
        "alphabet: " ++ show alphabet ++ "\n " ++
        "rules: " ++ show rules ++ "\n "
        
instance Show Rule where
    show (Rule currentState inputSymbol newState)=
        currentState ++ inputSymbol ++ " -> " ++ newState

parseArguments arguments = if length arguments < 1 || length arguments > 2 
    then error "Wrong parameters"
    else getArguments arguments

getArguments :: [String] -> Bool
getArguments [] = error "Missing argument"
getArguments [x]
    | x == "-i" = False    --- Just analyze and output DKA
    | x == "-t" = True     --- Output as MKA
    | otherwise = error "Unknown argument"
getArguments [x, _]
    | x == "-i" = False    --- Just analyze and output DKA
    | x == "-t" = True     --- Output as MKA
    | otherwise = error "Unknown argument"
getArguments _ = error "Wrong arguments"

-- decide if input is gonna be file or stdin
getInput arguments = if length arguments == 1
    then return stdin
    else openFile (last arguments) ReadMode

-- go through input (file) line by line and analyze incoming DKA
parseAutomat :: [[Char]] -> Automat
parseAutomat (states:start:end:transitionRules) = Automat{
    states = parseInputStates states,
    startingState = parseStartingState start,
    endStates = parseFinalStates end,
    alphabet = parseAlphabetFromRules transitionRules,
    rules = parseRules transitionRules
    }
parseAutomat others = error "invalid automat"

-- get all states from provided line (should be first line in input file)
parseInputStates :: [Char] -> Set.Set State
parseInputStates inputStates = Set.fromList $ splitOn "," inputStates

-- get starting state from provided line (should be second line in input file)
parseStartingState :: [Char] -> State
parseStartingState startingState = startingState

-- get final states from provided line (should be third line in input file)
parseFinalStates :: [Char] -> Set.Set State
parseFinalStates finalStates = Set.fromList $ splitOn "," finalStates

-- get alphabet from rules (rules should be from 4th line to the end in input file)
parseAlphabetFromRules :: [String] -> Alphabet
parseAlphabetFromRules rules = parseAlphabetFromRule rules Set.empty
    where
        parseAlphabetFromRule ([]:_) set = set
        parseAlphabetFromRule rules set = Set.fromList (map (\rule -> (splitOn "," rule) !! 1) rules)

-- get transition rules (rules should be from 4th line to the end in input file)
parseRules :: [String] -> [Rule]
parseRules rules = map (\rule -> readSingleRule rule) rules

-- get single rule from parseRules and process it
readSingleRule :: String -> Rule
readSingleRule rule = do
    let splittedRule = splitOn "," rule
    if length splittedRule /= 3
    then error "incorrect transition rule found"
    else Rule {
        currentState = splittedRule !! 0,
        inputSymbol = splittedRule !! 1,
        newState = splittedRule !! 2
    }

-- process created Automat data structure and preform validity checks for all elements
isAutomatValid :: Automat -> Bool
isAutomatValid automat = (all areStatesValid $ states automat) &&
                         (isStartingStateValid automat $ startingState automat) &&
                         (areEndingStatesValid automat) &&
                         (all (\rule -> areRulesValid automat rule) $ rules automat)

-- Return true if all states are valid, false otherwise
areStatesValid :: State -> Bool
areStatesValid states = all isDigit states

-- Return true if starting state is in set of states, false otherwise
-- note to myself: this is basically "is state in states", so i can as well use it that way
isStartingStateValid :: Automat -> State -> Bool
isStartingStateValid automat state = elem state $ states automat

-- return true if ending states are in set of states, false otherwise
areEndingStatesValid :: Automat -> Bool
areEndingStatesValid automat = all (\state -> isStartingStateValid automat state) $ endStates automat

-- return true if provided transition rule is valid, false otherwise
areRulesValid :: Automat -> Rule -> Bool
areRulesValid automat rule = (isStartingStateValid automat $ currentState rule) &&
                             (isSymbolValid (inputSymbol rule) automat) &&
                             (isStartingStateValid automat $ newState rule)

-- return true if provided symbol is in alphabet, false otherwise
isSymbolValid :: Symbol -> Automat -> Bool
isSymbolValid symbol automat = elem symbol $ alphabet automat

-- Remove unreachable states implemented by alg. 3.4 in TIN script
removeUnreachableStates :: Automat -> Automat
removeUnreachableStates automat = automat {
    states = Set.fromList reachableStates,
    endStates = Set.fromList reachableEndStates,
    rules = usefullRules
}where
    newStates = [startingState automat] -- Si = q0 
    reachableStates = nub $ getReachableStates [] newStates automat
    unreachableStates = Set.toList(states automat) \\ reachableStates
    reachableEndStates = [x | x <- (Set.toList (endStates automat)), x `elem` reachableStates]
    usefullRules = [x | x <- (rules automat), (currentState x) `elem` reachableStates]
    
getReachableStates :: [State] -> [State] -> Automat -> [State]
getReachableStates previousStates newStates automat = if previousStates == newStates
    then newStates
    else getReachableStates newStates (nub $ newStates ++ (getReachableStatesFromNewStates newStates automat)) automat

getReachableStatesFromNewStates :: [State] -> Automat -> [State]
getReachableStatesFromNewStates newStates automat = nub $ concat $ map (\state -> getReachableStatesFromState state $ rules automat) newStates

getReachableStatesFromState :: State -> [Rule] -> [State]
getReachableStatesFromState startingState rules = nub $ concat $ map (\rule -> getNewState startingState rule) rules where
    getNewState state rule = if ((currentState rule) == state)
    then [(newState rule)]
    else []

-- according to TIN script, sink state has to  be added in order to create MKA. New sink state has number "0" if added
addSinkToAutomat :: Automat -> Automat
addSinkToAutomat automat = if isSinkStateNeeded automat
    then automat {
        states = Set.insert "0" $ states automat,
        rules = rulesWithSinkState automat
    }
    else automat
    where
        automatRules automat = [(currentState, inputSymbol) | currentState <- (Set.toList $ states automat), inputSymbol <- (Set.toList $ alphabet automat)]
        allRulesPossible automat = [(currentState, inputSymbol) | currentState <- (Set.toList $ states automat) ++ ["0"], inputSymbol <- (Set.toList $ alphabet automat)]
        rulesWithSinkState automat = (rules automat) ++ (map (\rule -> createRule rule) ((allRulesPossible automat) \\ (automatRules automat)))
        createRule (state, symbol) = Rule {
            currentState = state,
            inputSymbol = symbol,
            newState = "0"
        }

-- perform check if sink is needed. The rule is states * alpha symbols should be equal transition rules. If not, sink is added
isSinkStateNeeded :: Automat -> Bool
isSinkStateNeeded automat = if (numberOfStates * numberOfSymbols) == numberOfTransitionRules
    then False
    else True
    where
        numberOfStates = Set.size $ states automat
        numberOfSymbols = Set.size $ alphabet automat
        numberOfTransitionRules = length $ rules automat

-- create final MKA
reduceAutomat :: Automat -> Automat
reduceAutomat automat = automat{
    states = reduceStates automat,
    startingState = getNewStartingState automat,
    endStates = reduceEndStates automat,
    rules = reduceRules automat
} where
    -- P = {F, Q \ F}
    -- W = {F}
    p = Set.fromList [endStates automat, Set.difference (states automat) (endStates automat)]
    w = Set.singleton (endStates automat)
    reduceStates automat = Set.map (\state -> (Set.toList state) !! 0) reducedStates
    getNewStartingState automat = (Set.toList newStartingState) !! 0
    reduceEndStates automat = Set.map (\state -> (Set.toList state) !! 0) reducedEndStates
    reduceRules automat = getReducedRules (rules automat) reducedStates
    reducedStates = hopcroft p w (alphabet automat) (rules automat)
    newStartingState = getNewStartingState' (startingState automat) reducedStates
    reducedEndStates = getReducedEndStates (endStates automat) reducedStates

-- hopcroft algorithm implemented as described at DFA minimization wikipedia  
-- while (W is not empty) DO  
hopcroft :: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Set.Set Symbol -> [Rule] -> Set.Set(Set.Set State)
hopcroft p w alphabet rules = if (Set.size w) /= 0
    then hopcroft newP newW alphabet rules
    else p
    where
        (newP, (a, newW)) = hopcroftForEachC (p, Set.deleteFindMin w) alphabet rules    -- choose and remove set A from W

-- For each C in alphabet DO
hopcroftForEachC :: (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State))) -> Alphabet -> [Rule] -> (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State)))
hopcroftForEachC (p, (a, w)) alphabet rules = if Set.size alphabet /= 0
    then hopcroftForEachC (hopcroftModifySets (p, (a, w)) c rules) c1 rules
    else (p, (a, w))
    where
        (c, c1) = Set.deleteFindMin alphabet

-- for each set Y in P for which X intersection Y is nonempty and Y \ X is not empty DO
hopcroftModifySets :: (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State))) -> Symbol -> [Rule] -> (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State)))
hopcroftModifySets (p, (a, w)) c rules = do
    let setX = hopcroftGetX a c rules 
    let setY = hopcroftGetY p setX
    let tmpY = getDifferenceInSets p setY
    (hopcroftReplaceYinP setY tmpY setX, (a, hopcroftReplaceYinW setY setX w))

hopcroftReplaceYinP :: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)        
hopcroftReplaceYinP setY tmpY setX = Set.union tmpY (hopcroftReplaceSetYinP setY setX)

hopcroftReplaceSetYinP :: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
hopcroftReplaceSetYinP setY setX = Set.union (Set.map (\y -> Set.intersection setX y) setY) (Set.map (\y -> getDifferenceInSets y setX) setY)

hopcroftReplaceYinW :: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State) -> Set.Set(Set.Set State)
hopcroftReplaceYinW setY setX setW = Set.union setW (Set.map ifYinW setY)
    where ifYinW y = if (elem y setW)
                     then hopcroftReplaceYinW' setX y   -- IF Y is in W
                     else hopcroftAddResultToW setX y   -- ELSE

-- replace Y in W by same two sets
hopcroftReplaceYinW' setX setY = Set.union (Set.intersection setX setY) (getDifferenceInSets setY setX)

-- ELSE Y not inn W
hopcroftAddResultToW setX setY = if (Set.size (Set.intersection setX setY) <= Set.size (getDifferenceInSets setY setX))
    then Set.intersection setX setY -- add X intersection Y to W
    else getDifferenceInSets setY setX -- ADD Y \ X to W  

hopcroftGetY :: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
hopcroftGetY p setX = Set.filter (\setY -> (doesSetConatinsSomething (Set.intersection setX setY) && doesSetConatinsSomething (getDifferenceInSets setY setX))) p

hopcroftGetX :: Set.Set State -> Symbol -> [Rule] -> Set.Set State
hopcroftGetX requestedSets symbol rules = getSetOfRulesLeadingToSetsInA (getRulesWithNewStates (getRulesWithSymbol rules symbol) requestedSets)

getNewStartingState' :: State -> Set.Set(Set.Set State) -> Set.Set State
getNewStartingState' currentStartingState reducedStates = Set.elemAt 0 $ Set.filter (\state -> Set.member currentStartingState state) reducedStates

getReducedEndStates :: Set.Set State -> Set.Set(Set.Set State) -> Set.Set(Set.Set State)
getReducedEndStates currentEndstates reducedStates = Set.filter (\state -> doesSetConatinsSomething $ Set.intersection state currentEndstates) reducedStates

getReducedRules :: [Rule] -> Set.Set(Set.Set State) -> [Rule]
getReducedRules currentRules reducedStates = nub $ map (\rule -> (getReducedRules' rule reducedStates)) currentRules

getReducedRules' :: Rule -> Set.Set(Set.Set State) -> Rule
getReducedRules' rule reducedStates = Rule{
    currentState = getNewState (currentState rule) reducedStates,
    inputSymbol = inputSymbol rule,
    newState = getNewState (newState rule) reducedStates
} where
    getNewState currentState reducedStates = (Set.toList $ getFirstElement $ Set.filter (\state -> (Set.member currentState state)) reducedStates) !! 0
    getFirstElement set = Set.elemAt 0 $ set

getDifferenceInSets setX setY = Set.difference setX setY

doesSetConatinsSomething :: Set.Set State -> Bool
doesSetConatinsSomething setX = not $ null setX

getRulesWithSymbol :: [Rule] -> Symbol -> [Rule]
getRulesWithSymbol rules symbol = filter (\rule -> (inputSymbol rule == symbol)) rules

getRulesWithNewStates :: [Rule] -> Set.Set State -> [Rule]
getRulesWithNewStates rules states = filter (\rule -> (elem (newState rule) states)) rules

getSetOfRulesLeadingToSetsInA :: [Rule] -> Set.Set State
getSetOfRulesLeadingToSetsInA filteredRules = Set.fromList $ map (\rule -> (currentState rule)) filteredRules
    
-- output automat to stdout
printKA :: Automat -> IO()
printKA automat = do
    putStrLn $ concat $ intersperse "," $ Set.toList $ states automat   -- print all states
    putStrLn $ startingState automat    -- print starting state
    putStrLn $ concat $ intersperse "," $ Set.toList $ endStates automat -- print all ending states
    putStrLn $ concat $ intersperse "\n" $ map (\rule -> printRule rule) $ rules automat        -- print all rules

-- help function for printKA, to print rules
printRule :: Rule -> String
printRule rule = currentState rule++ "," ++ inputSymbol rule ++ "," ++ newState rule
 
main :: IO() 
main = do
    arguments <- getArgs
    --print arguments
    let doReduce = getArguments arguments     --select if i have to reduce the automat or just print it
    
    input <- getInput arguments         -- read from file or stdin?
    content <- hGetContents input       -- read
    --putStr content
    
    let dka = parseAutomat $ lines content
    --print (show dka)
    when (not $ isAutomatValid dka) $ error "This is not valid automat"
    
    if doReduce
    then printKA $ reduceAutomat $ addSinkToAutomat $ removeUnreachableStates dka          -- output MKA
    else printKA dka     -- output analyzed DKA
    
    hClose input