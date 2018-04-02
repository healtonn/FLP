import System.Environment
import System.IO
import Data.List
import Data.Char
                                      
            
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
 
main :: IO() 
main = do
    arguments <- getArgs
    print arguments
    let reduce = getArguments arguments     --select if i have to reduce the automat or just print it
    input <- getInput arguments         -- read from file or stdin?
    content <- hGetContents input       -- read
    putStr content
    
    hClose input
    

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"