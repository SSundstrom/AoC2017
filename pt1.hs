import Parsers

prel :: [Int] -> Int
prel (x:xs) = sum $ findDups ((x:xs) ++ [x])

findDups :: [Int] -> [Int]
findDups (x:y:xs) = if x == y 
    then x : findDups (y:xs)
    else findDups (y:xs)
findDups _ = []

 -- **************************

prel1 :: [Int] -> Int
prel1 s = sum $ findHalfwayDups fstHalf sndHalf
    where (fstHalf, sndHalf) = splitAt (div (length s) 2) s

findHalfwayDups :: [Int] -> [Int] -> [Int]
findHalfwayDups (x:xs) (y:ys) = if x == y
    then x * 2 : findHalfwayDups xs ys
    else findHalfwayDups xs ys
findHalfwayDups _ _ = []