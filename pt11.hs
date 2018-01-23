module Main where
import Parsers
import Data.List.Split
import Data.List
import Data.Char (isSpace)

main = do
    input <- getInput 11
    let input' = splitOn "," $ trim input
    timed $ ptA input'
    timed $ ptB input'

data Grd3D = Grd3D {x::Int, y::Int, z::Int}
baseGrid = Grd3D {x=0, y=0, z=0}

change :: Grd3D -> (Int, Int, Int) -> Grd3D
change g (x', y', z') = Grd3D {x = x g + x', y = y g + y', z = z g + z'}

distance :: Grd3D -> Int
distance pos = (abs (x pos) + abs (y pos) + abs (z pos)) `div` 2

ptA :: [String] -> Int
ptA = distance . foldl move baseGrid

ptB :: [String] -> Int
ptB = fst . foldl (\ (x, y) z -> (max x (distance y), move y z)) (0, baseGrid)

move :: Grd3D -> String -> Grd3D
move g inst = case inst of 
    "n" -> change g (1, 1, 0)
    "s" -> change g (-1, -1, 0)
    "ne" -> change g (0, 1, 1)
    "sw" -> change g (0, -1, -1)
    "nw" -> change g (1, 0, -1)
    "se" -> change g (-1, 0, 1)
    _ -> error ("Wrong parse '" ++ inst ++ "'")

trim = dropWhileEnd isSpace . dropWhile isSpace