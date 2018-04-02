import System.Environment
import System.IO
import Data.List
import Data.Char
import qualified Data.Set as Set
                                      

-- naming stuff
type State = String
type Symbol = String
type Alphabet = Set.Set Symbol

-- automat rules
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
    alphabet :: Alphabet
}
                                      
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

getInput arguments = if length arguments == 1
    then return stdin
    else openFile (last arguments) ReadMode
    
printDKA automat = do 
    print "DKA"
    let line = lines automat
    print line

printMKA automat = do 
    print "MKA"
 
main :: IO() 
main = do
    arguments <- getArgs
    print arguments
    let doReduce = getArguments arguments     --select if i have to reduce the automat or just print it
    input <- getInput arguments         -- read from file or stdin?
    content <- hGetContents input       -- read
    putStr content
    
    if doReduce
    then printMKA content          -- output MKA
    else printDKA content     -- output analyzed DKA
    
    hClose input
    

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"