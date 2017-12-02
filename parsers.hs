module Parsers where
import Data.Char
import Network.Curl
import Data.Maybe
import Text.Regex

nums = mkRegex "[0-9]+"

getInput :: Integer -> IO String
getInput day = do
    cookie <- readFile ".cookie"
    (cc, s) <- curlGetString (concat ["http://adventofcode.com/2017/day/", show day, "/input"]) [CurlCookie cookie]
    return s

intsByRowWord :: Integer -> IO [[Int]]
intsByRowWord day = do
    input <- getInput day
    let parsedString = map words $ lines input
    let parsedInt = map (map (\x -> read x ::Int)) parsedString
    return parsedInt

intsFromString :: Integer -> IO [Int]
intsFromString day = do
    c <- getInput day
    return $ map digitToInt $ filter (isJust . matchRegex nums . show ) c
