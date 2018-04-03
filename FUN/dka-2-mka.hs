import System.Environment
import System.IO
import Data.List
import Data.Char
import Data.List.Split
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
}

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

-- get single rule from parseAlphabetFromRules and process it
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
    print arguments
    let doReduce = getArguments arguments     --select if i have to reduce the automat or just print it
    input <- getInput arguments         -- read from file or stdin?
    content <- hGetContents input       -- read
    putStr content
    let dka = parseAutomat $ lines content
    print (show dka)
    
    if doReduce
    then printKA dka          -- output MKA
    else printKA dka     -- output analyzed DKA
    
    hClose input
    

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"