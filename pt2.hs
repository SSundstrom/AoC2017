module Pt2 where
import Parsers
import Data.List

-- Less pleb solution

minmaxSum :: [[Int]] -> Int
minmaxSum l = sum $ map (\x -> maximum x - minimum x) l

dividableSum :: [[Int]] -> Int
dividableSum l = sum $ map (divisible . sortBy (flip compare)) l

divisible :: [Int] -> Int
divisible [] = 0
divisible (x:xs) = divisible xs + (sum $ map (\y -> if rem y x == 0 then div y x else 0) xs)


 -- ***** Pleb solution


{-
prel1 :: String -> Int
prel1 content = checkSum $ map words (lines content)

checkSum :: [[String]] -> Int
checkSum l = sum $ map (\(x:xs) -> findDiff xs (read x::Int) (read x::Int)) l

findDiff :: [String] -> Int -> Int -> Int
findDiff [] min max = max - min 
findDiff (x:xs) min max
    | i > max && i < min = findDiff xs i i
    | i > max = findDiff xs min i
    | i < min = findDiff xs i max
    | otherwise = findDiff xs min max
    where i = read x :: Int

divisions :: [Int] -> Int
divisions l = sum $ division' l l

division' :: [Int] -> [Int] -> [Int]
division' [] _ = []
division' (x:xs) l = map dividable [(x, y) | y <- l] ++ division' xs l

prel2 :: String -> Int
prel2 s = sum $ map ((divisions . map (\ y -> read y :: Int)) . words) (lines s) 

dividable :: (Int, Int) -> Int
dividable (x, y) 
    | x == y = 0
    | rem x y == 0 = div x y
    | otherwise = 0
-} 