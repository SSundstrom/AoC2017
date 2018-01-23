module Main where
import Parsers
import Data.Maybe

main = do
    input2 <- readFile "test"
    let input' = map words . lines $ input2
    input' <- linesWords 13
    let input = map (mapMaybe cleanInt) input'
    timed $ ptA input
    timed $ ptB input

ptA :: [[Int]] -> Int
ptA = foldl step 0 . map (\x -> (0, x))

step :: Int -> (Int,[Int]) -> Int
step val (del, d : (r : xs)) = if (d+del) `mod` ((r-1) * 2) == 0 then val + d*r else val
step val _ = error "Wrong amount of numbers"

ptB :: [[Int]] -> Int
ptB = lowestMod 0

lowestMod :: Int -> [[Int]] -> Int
lowestMod delay wall = if foldl step 0 [(delay, s) | s <- wall] > 0 
                                || caughtIn0 delay wall
                        then lowestMod (delay + 1) wall
                        else delay

caughtIn0 :: Int -> [[Int]] -> Bool
caughtIn0 del (x:xs) = del `mod` (((x!!1)-1) * 2) == 0
caughtIn0 del _ = error "shouldn't happen"

test :: [[Int]]
test = [[0,3], [1,2], [4,4], [6,4]]