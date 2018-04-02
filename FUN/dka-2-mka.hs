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
    endStates :: Set.Set State
    --alphabet :: Alphabet,
    --rules :: [Rule]
}

-- for debugging
instance Show Automat where
    show (Automat states startingState endStates) =  
        "states: " ++ show states ++ "\n " ++
        "startingState: " ++ show startingState ++ "\n " ++
        "endingStates: " ++ show endStates ++ "\n "

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
parseAutomat (states:start:end:rules) = Automat{
    states = parseInputStates states,
    startingState = parseStartingState start,
    endStates = parseFinalStates end
    }
parseAutomat others = error "invalid automat"

parseInputStates inputStates = Set.fromList $ splitOn "," inputStates

parseStartingState startingState = startingState

parseFinalStates finalStates = Set.fromList $ splitOn "," finalStates
    
printDKA inputAutomat = do 
    print "DKA"

printMKA inputAutomat = do 
    print "MKA"
 
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
    then printMKA dka          -- output MKA
    else printDKA dka     -- output analyzed DKA
    
    hClose input
    

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"