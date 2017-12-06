import Parsers
import Data.Sequence as S hiding (filter)
import Data.Maybe 

test = S.fromList [0, 2, 7 ,0]

pt1test = blocks test (findMaxi test 0) []

pt1 :: IO ()
pt1 = do
    input <- intsByWord 6
    let sIn = S.fromList input
    timed (blocks sIn (findMaxi sIn 0) [])

pt2 :: IO ()
pt2 = do
    input <- intsByWord 6
    let sIn = S.fromList input
    timed (blocks' sIn (findMaxi sIn 0) [] 0)

    
blocks :: Seq Int -> Int -> [Seq Int] -> Int
blocks curr i old 
    | curr `elem` old = 0
    | otherwise = 1 + blocks updated (findMaxi updated 0) (curr : old)
    where updated = update' tookValue nxti (index curr i) (S.length curr)
          tookValue = update i 0 curr
          nxti = rem (i + 1) (S.length curr)

blocks' :: Seq Int -> Int -> [(Int, Seq Int)] -> Int -> [Int]
blocks' curr i old iter
    | curr `elem` map snd old = map (\x -> fst x - iter) $ filter (\x -> curr == snd x) old
    | otherwise = blocks' updated (findMaxi updated 0) ((iter, curr) : old) (iter+1)
    where updated = update' tookValue nxti (index curr i) (S.length curr)
          tookValue = update i 0 curr
          nxti = rem (i + 1) (S.length curr)

findMaxi :: Seq Int -> Int -> Int
findMaxi s i 
    | S.length s == i+1 = i
    | otherwise = if index s i >= index s maxi then i else maxi
    where maxi = findMaxi s (i+1)

update' :: Seq Int -> Int -> Int -> Int -> Seq Int
update' og i v s
    | s == 0 = og
    | otherwise = update' (update i newV og) nxti (v-x) (s-1)
    where x = div v s + (if rem v s == 0 then 0 else 1)
          nxti = rem (i + 1) (S.length og)
          newV = index og i + x


-- Måste se över

