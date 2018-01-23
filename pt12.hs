module Main where
import Parsers
import Data.Maybe (mapMaybe)
import Text.Regex
import Data.Graph

main = do
    input <- getInput 12
    timed $ ptA . lines $ input
    timed $ ptB . lines $ input


test = do
    input <- readFile "test"
    timed $ ptA . lines $ input
    timed $ ptB . lines $ input


ptA :: [String] -> Int
ptA = length . (`reachable` 0) . makeGraph 

makeGraph :: [String] -> Graph
makeGraph things = buildG (0, length things - 1) $ foldr ((++) . makeEdges) [] things

makeEdges :: String -> [(Int, Int)]
makeEdges input = [(id, conn) | conn <- conns]
    where 
        id = head id'
        (id', conns) = splitAt 1 $ map (strToInt . head ) $ mapMaybe (matchRegex nums') $ words input
        
ptB :: [String] -> Int
ptB = length . components . makeGraph