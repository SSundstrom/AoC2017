module Main where
import Parsers
import Data.IntMap
    
main = both


pt1 :: IO ()
pt1 = do
    input <- intsByWord 5
    let imap = fromList (zip [x | x <- [0..length input]] input)
    print "Part 1"
    timed (step (size imap) 0 imap)
    

pt2 :: IO ()
pt2 = do
    input <- intsByWord 5
    let imap = fromList (zip [x | x <- [0..length input]] input)
    timed (step' (size imap) 0 imap)
    


both :: IO ()
both = do
    input <- intsByWord 5
    let imap = fromList (zip [x | x <- [0..length input]] input)
    print "Part 1"
    timed $ step (size imap) 0 imap
    print "Part 2"
    timed $ step' (size imap) 0 imap
    


step :: Int -> Int -> IntMap Int -> Int
step s i l 
    | s <= i = 0
    | otherwise = 1 + step s (i + v) (update (\x -> Just (x+1)) i l)
    where v = l!i

step' :: Int -> Int -> IntMap Int -> Int
step' s i l 
    | s <= i = 0
    | v > 2 = 1 + step' s (i + v) (update (\x -> Just (x-1)) i l)
    | otherwise = 1 + step' s (i + v) (update (\x -> Just (x+1)) i l)
    where v = l!i
    
{-
incElem :: Int -> [Int] -> [Int]
incElem i l = concat [x, [(y!!0)+1], drop 1 y]
    where (x, y) = splitAt i l

decElem :: Int -> [Int] -> [Int]
decElem i l = concat [x, [(y!!0)-1], drop 1 y]
    where (x, y) = splitAt i l
-}