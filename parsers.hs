module Parsers where
import Data.Char

intsByRowWord :: ([[Int]] -> Int) -> FilePath -> IO ()
intsByRowWord f p = do
    c <- readFile p
    let parsedString = map words $ lines c
    let parsedInt = map (map (\x -> read x ::Int)) parsedString
    print $ f parsedInt

intsFromString :: ([Int] -> Int) -> FilePath -> IO ()
intsFromString f p = do
    c <- readFile p
    print $ f $ map digitToInt c