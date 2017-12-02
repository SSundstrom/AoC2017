module Parsers where

intsByRowWord :: ([[Int]] -> Int) -> IO ()
intsByRowWord f = do
    c <- readFile "input"
    let parsedString = map words $ lines c
    let parsedInt = map (map (\x -> read x ::Int)) parsedString
    print $ f parsedInt