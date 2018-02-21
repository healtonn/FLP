import System.Environment
import Data.List
                                       
parseArguments arguments = if length arguments < 1 || length arguments > 2 
    then "Wrong parameters"
    else do show(arguments) ++ " pohoda"
            --let validArguments = ["-i", "-t"]
            --debugPrintElements validArguments

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x] = error "list must have at least two elements"
myButLast [] = error "empty list"
myButLast (x:xs) = do if length xs == 1
                      then x
                      else myButLast xs
                      
elementAt :: [a] -> Int -> a
elementAt [] k = error "empty list"
elementAt (x:_) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x =  (head x) == (last x) && (isPalindrome $ init $ tail x)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x
            
main = do
    --arguments <- getArgs
    --print arguments
    --print (parseArguments arguments)
    print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    print (isPalindrome "madammadam")
    print (isPalindrome [1,2,4,8,16,8,4,2,1])
    return()