doubleMe :: Integer -> Integer
doubleMe x = x + x 

doubleUs x y = doubleMe x + doubleMe y 

doubleSmallNumber x = if x > 100
                        then x  
                        else x*2 

doubleSmallNumber' x = (if x > 100 then x else x*2)

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

triangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b] ]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

listOfTupples = [(1, 2), (2, 3), (3, 4), (4, 5)]

test1 testList = [ x + y | (x,y ) <- testList]

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 
          
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

--test listOfTupples

emptyListError = error "empty List !"

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

compress :: Eq a => [a] -> [a]
compress = map head . group

takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x:xs)
        | n <= 0 = []
        | True = x : takeN (n-1) xs
                    
dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN n (x:xs)
        | n <= 1 = xs
        | True = dropN (n-1) xs
        
replac :: Int -> Char -> Char -> [String] -> [String]
replac _ _ _ [] = []
replac n a b (x:xs) = (start ++ swap konec) : (replac (n-1) a b xs)
        where
        start = takeN n x
        konec = dropN n x
        swap [] = []
        swap (c:cs) = if c == a then (b:cs) else (c:cs)


main = print(test1 listOfTupples)
    --print (takeN 3 [1,2,3,4,5,6])
    --print (dropN 3 [1,2,3,4,5,6])
    --print (replac 2 'a' 'b' ["aaaa", "aaaaa", "aaaa", "bbbb"])