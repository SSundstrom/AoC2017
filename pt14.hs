module Main where
import Parsers
import Pt10 
import Data.Char
import Data.Foldable
import Numeric (readHex)
import Text.Printf (printf)
import Data.Maybe
import Control.Monad

main = do
    let input = "hxtvlmkl-"
    -- timed $ ptA [ input ++ show x | x <- [0..127] ]
    mapM_ putStrLn $ Main.ptB [ input ++ show x | x <- [0..127] ]

ptA :: [String] -> Int
ptA = length . filter ('1'==) . concatMap ( concat . mapMaybe hexToBin . Pt10.ptB . map ord)


hexToBin :: Char -> Maybe String
hexToBin c
  = case readHex [c] of
      (x,_):_ -> Just $ printf "%04b" (x::Int)
      _       -> Nothing

ptB :: [String] -> [String]
ptB = map (concat . mapMaybe hexToBin . Pt10.ptB . map ord)