module Main where

import Parsers
import Data.List

main = pt9

pt9 :: IO()
pt9 = do
    input <- getInput 9
    -- timed $ ptA input
    timed $ ptB input

ptA :: String -> Int
ptA i = s $ foldl readString empty i

ptB :: String -> Int
ptB i = gc $ foldl readString empty i

data Stream = Stream {s::Int, cb::Int, g::Bool, i::Bool, gc::Int}

empty :: Stream
empty = Stream {s=0, cb=1, g=False, i=False, gc=0}


readString :: Stream -> Char -> Stream
readString strm c
    | i strm = strm {i=False}
    | c == '!' = strm {i=True} 
    | g strm = case c of
        '!' -> strm {i = True}
        '>' -> strm {g = False}
        _ -> strm {gc = gc strm + 1}
    | otherwise = case c of
        '<' -> strm {g = True}
        '{' -> strm {cb = cb strm + 1, s = s strm + cb strm}
        '}' -> strm {cb = cb strm - 1}
        _ -> strm