module Main where
import Parsers
import Data.Map.Strict as A
import Data.IntMap as B
    
main = both

pt1 :: IO ()
pt1 = do
    imap <- getintMap
    print "Part 1"
    timed (step (A.size imap) 0 imap)
    

pt2 :: IO ()
pt2 = do
    imap <- getintMap
    timed (step1i (A.size imap) 0 imap)
    


both :: IO ()
both = do
    imap <- getintMap
    putStrLn "Part 1"
    timed $ step (A.size imap) 0 imap
    putStrLn "Part 2"
    putStrLn "--------------------"
    putStrLn "map insert"
    timed $ step1i (A.size imap) 0 imap
    putStrLn "--------------------"    
    putStrLn "map update"
    timed $ step1u (A.size imap) 0 imap
    putStrLn "--------------------"    
    iimap <- getIntMap
    putStrLn "intMap insert"
    timed $ step2i (B.size iimap) 0 iimap
    putStrLn "--------------------"    
    putStrLn "intMap update"
    timed $ step2u (B.size iimap) 0 iimap
    
getintMap :: IO (Map Int Int)
getintMap = do
    input <- intsByWord 5
    return $ A.fromList (zip [0 .. length input] input)

getIntMap :: IO (IntMap Int)
getIntMap = do
    input <- intsByWord 5
    return $ B.fromList (zip [0 .. length input] input)
    

step :: Int -> Int -> Map Int Int -> Int
step s i l 
    | s <= i = 0
    | otherwise = 1 + step s (i + v) (A.update (\x -> Just (x+1)) i l)
    where v = l A.! i

step1u :: Int -> Int -> Map Int Int -> Int
step1u s i l 
    | s <= i = 0
    | v > 2 = 1 + step1u s (i + v) (A.update (\x -> Just (x-1)) i l)
    | otherwise = 1 + step1u s (i + v) (A.update (\x -> Just (x+1)) i l)
    where v = l A.! i
    
step1i :: Int -> Int -> Map Int Int -> Int
step1i s i l 
    | s <= i = 0
    | v > 2 = 1 + step1i s (i + v) (A.insert i (v-1) l)
    | otherwise = 1 + step1i s (i + v) (A.insert i (v+1) l)
    where v = l A.! i

step2u :: Int -> Int -> IntMap Int -> Int
step2u s i l 
    | s <= i = 0
    | v > 2 = 1 + step2u s (i + v) (B.update (\x -> Just (x-1)) i l)
    | otherwise = 1 + step2u s (i + v) (B.update (\x -> Just (x+1)) i l)
    where v = l B.! i


step2i :: Int -> Int -> IntMap Int -> Int
step2i s i l 
    | s <= i = 0
    | v > 2 = 1 + step2i s (i + v) (B.insert i (v-1) l)
    | otherwise = 1 + step2i s (i + v) (B.insert i (v+1) l)
    where v = l B.! i
    

{-
incElem :: Int -> [Int] -> [Int]
incElem i l = concat [x, [(y!!0)+1], drop 1 y]
    where (x, y) = splitAt i l

decElem :: Int -> [Int] -> [Int]
decElem i l = concat [x, [(y!!0)-1], drop 1 y]
    where (x, y) = splitAt i l
-}