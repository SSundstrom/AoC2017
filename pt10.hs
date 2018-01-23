module Pt10 (initList, List, step, list, restoreHead, makeHex, ptB) where
import Parsers
import Data.Sequence as S
import Data.Bits
import Numeric
import Data.Char
import Text.Printf
import Data.Word (Word8)

initSeq = fromList [0..255]

data List = List {list :: Seq Int, hPos::Word8, sVal::Word8}

initList = List {list = initSeq, hPos = 0, sVal = 0}

main = do
    input <- intsCSV 10
    timed $ ptA input
    input2 <- inputAsAscii 10
    timed $ ptB input2

ptA :: [Int] -> Seq Int
ptA = S.take 2 . list . restoreHead . foldl step initList 

ptB :: [Int] -> String
ptB = makeHex . makeDense . list . restoreHead . foldl step initList . concat . S.replicate 64 . (++bonus)
        where
            bonus = [17, 31, 73, 47, 23]

step :: List -> Int -> List
step lst i = List {list = skip (sVal lst) . (S.drop i (list lst) ><) . S.reverse . S.take i . list $ lst, 
                    hPos = hPos lst - sVal lst - fromIntegral i,
                    sVal = sVal lst + 1}

makeDense :: Seq Int -> [Int]
makeDense seq 
    | S.null seq = []
    | otherwise = foldr xor 0 front : makeDense end
        where
            (front, end) = S.splitAt 16 seq

makeHex :: [Int] -> String
makeHex = concatMap (printf "%02x")

restoreHead :: List -> List
restoreHead lst = lst { list = (\(x, y) -> y >< x) $ S.splitAt (fromIntegral (hPos lst)) (list lst), hPos = 0}

skip :: Word8 -> Seq a -> Seq a
skip n seq = tail >< head
    where (head, tail) = S.splitAt (fromIntegral n) seq
