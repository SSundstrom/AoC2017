module Parsers where
import Data.Char
import Network.Curl
import Data.Maybe
import System.CPUTime
import Text.Printf
import Text.Regex
import Control.Parallel
-- import Data.String.Utils

nums = mkRegex "[0-9]+"
nums' = mkRegex "([0-9]+)"

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
    let diff = fromIntegral (end - start) / (10 ^ 12)
    putStrLn $ "Answer = " ++ show f
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
    return $ map digitToInt $ filter (isJust . matchRegex nums . show) c

cleanInt :: String -> Maybe Int
cleanInt = fmap (strToInt . head) . matchRegex nums'

intsByWord :: Integer -> IO [Int]
intsByWord day = do
    c <- getInput day
    let parsedString = words c
    let parsedInt = map strToInt parsedString
    return parsedInt

intsCSV :: Integer -> IO [Int]
intsCSV day = do
    s <- getInput day
    let rs = s -- replace "," " " s
    return $ map strToInt $ words rs

getAllInputs :: Integer -> IO()
getAllInputs 25 = writeInputToFile 25
getAllInputs day = do
    writeInputToFile day
    getAllInputs $ day+1

writeInputToFile :: Integer -> IO()
writeInputToFile day = do 
    input <- getInput day
    let path = "inputs/day" ++ show day
    writeFile path input

inputAsAscii :: Integer -> IO [Int]
inputAsAscii day = do
    input <- getInput day
    return $ map ord . init $ input
