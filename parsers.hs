module Parsers where
import Data.Char
import Network.Curl
import Data.Maybe
import System.CPUTime
import Text.Printf
import Text.Regex
import Control.Parallel

nums = mkRegex "[0-9]+"

getInput :: Integer -> IO String
getInput day = do
    cookie <- readFile ".cookie"
    (cc, s) <- curlGetString (concat ["http://adventofcode.com/2017/day/", show day, "/input"]) [CurlCookie cookie]
    return s

strToInt :: String -> Int
strToInt = read

timed :: Show a => a -> IO ()
timed f = do
    start <- getCPUTime
    let end' = pseq f getCPUTime
    end <- end'
    let diff = (fromIntegral (end - start)) / (10^12)
    print $ "Answer = " ++ show f
    printf "Computation time: %0.3f ms\n" ((diff :: Double) * 1000)

intsByRowWord :: Integer -> IO [[Int]]
intsByRowWord day = do
    input <- getInput day
    let parsedString = map words $ lines input
    let parsedInt = map (map strToInt) parsedString
    return parsedInt

linesWords :: Integer -> IO [[String]]
linesWords day = do
    input <- getInput day
    return $ map words (lines input)

intsFromString :: Integer -> IO [Int]
intsFromString day = do
    c <- getInput day
    return $ map digitToInt $ filter (isJust . matchRegex nums . show ) c

intsByWord :: Integer -> IO [Int]
intsByWord day = do
    c <- getInput day
    let parsedString = lines c
    let parsedInt = map strToInt parsedString
    return parsedInt

readNegative :: String -> Int
readNegative x = read x ::Int