import Parsers
import System.Environment
import Data.Time
import Data.List

valid :: [String] -> Int
valid [] = 1
valid (x:xs) = if (not (elem x xs) && 1 == valid xs) then 1 else 0

pt1and2 :: IO ()
pt1and2 = do 
    ms <- linesWords 4
    start' <- getCurrentTime
    let ans = sum $ map (valid) ms
    end' <- getCurrentTime
    print $ show ans
    print $ diffUTCTime end' start'
    start <- getCurrentTime
    let ans = sum $ map valid (map (map sort) ms)
    end <- getCurrentTime
    print $ show ans
    print $ diffUTCTime end start


    -- **

pt1 :: IO ()
pt1 = do
    ms <- linesWords 4
    print $ show $ sum $ map (valid) ms

pt2 :: IO ()
pt2 = do
    ms <- linesWords 4
    start <- getCurrentTime
    let ans = sum $ map (valid) (map (map sort) ms)
    stop <- getCurrentTime
    print $ show ans
    print $ diffUTCTime stop start

pt2' :: IO ()
pt2' = do
    ms <- linesWords 4
    start <- getCurrentTime
    let ans = sum $ map (anaValid) (ms)
    stop <- getCurrentTime
    print $ show ans
    print $ diffUTCTime stop start

pt1and2 :: IO ()
pt1and2 = do 
    
    start' <- getCurrentTime
    ms <- linesWords 4
    end' <- getCurrentTime
    print $ diffUTCTime end' start'
    
    start' <- getCurrentTime
    let ans = sum $ map (valid) ms
    end' <- getCurrentTime
    print $ show ans
    print $ diffUTCTime end' start'
    
    start <- getCurrentTime
    let ans = sum $ map (anaValid) ms
    end <- getCurrentTime
    print $ show ans
    print $ diffUTCTime end start


    start <- getCurrentTime
    let ans = sum $ map valid (map (map sort) ms)
    end <- getCurrentTime
    print $ show ans
    print $ diffUTCTime end start


    

anaValid :: [String] -> Int
anaValid [] = 1
anaValid (x:xs) = if not (any (isAna x) xs) && anaValid xs == 1 then 1 else 0

isAna :: String -> String -> Bool
isAna [] [] = True
isAna (x:xs) y
    | length (x:xs) == length y = if elem x y then isAna xs (delFirstOcc x y) else False
    | otherwise = False

delFirstOcc :: Char -> String -> String
delFirstOcc c (x:xs)
    | c == x = xs
    | otherwise = x : delFirstOcc c xs