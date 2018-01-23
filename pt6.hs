module Main where
import Parsers
import Data.Sequence as S
import Data.Maybe 


main = pt1

test :: Seq Int
test = S.fromList [0, 2, 7 ,0]

-- pt1test = blocks test (findMaxi test 0)

pt1 :: IO ()
pt1 = do
    input <- intsByWord 6
    let sIn = S.fromList input
    timed $ S.length (blocks sIn S.empty)

pt2 :: IO ()
pt2 = do
    input <- intsByWord 6
    let sIn = S.fromList input
    let ans1 = blocks sIn S.empty
    timed $ S.length $ blocks (index ans1 0) S.empty
    
blocks :: Seq Int -> Seq (Seq Int) -> Seq (Seq Int)
blocks curr old
    | curr `elem` old = curr <| old
    | otherwise = blocks updated (curr <| old)
    where 
        i = findMaxi curr 0
        nexti = rem (i + 1) (S.length curr)
        emptyI = update i 0 curr
        updated = update' emptyI nexti (index curr i) (S.length curr)
            
findMaxi :: Seq Int -> Int -> Int
findMaxi s i
    | S.length s == i = 0
    | otherwise = if index s i >= index s j then i else j
    where 
        j = findMaxi s (i+1)

update' :: Seq Int -> Int -> Int -> Int -> Seq Int
update' og i v s
    | s == 0 = og
    | otherwise = update' (update i newV og) nxti (v-x) (s-1)
    where x = div v s + (if rem v s == 0 then 0 else 1)
          nxti = rem (i + 1) (S.length og)
          newV = index og i + x


-- Måste se över

