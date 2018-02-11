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

--test listOfTupples


main = print(test1 listOfTupples)