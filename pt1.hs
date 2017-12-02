module Pt1 where
import Parsers

pt1 :: IO ()
pt1 = do
    (x:xs) <- intsFromString 1
    print $ sum $ findDups ((x:xs) ++ [x])

findDups :: [Int] -> [Int]
findDups (x:y:xs) = if x == y 
    then x : findDups (y:xs)
    else findDups (y:xs)
findDups _ = []

 -- **************************

pt2 :: IO ()
pt2 = do
    input <- intsFromString 1
    let halvedInput = splitAt (div (length input) 2) input
    print $ sum  $ findHalfwayDups halvedInput

findHalfwayDups :: ([Int], [Int]) -> [Int]
findHalfwayDups ((x:xs), (y:ys)) = if x == y
    then x * 2 : findHalfwayDups (xs, ys)
    else findHalfwayDups (xs, ys)
findHalfwayDups _ = []