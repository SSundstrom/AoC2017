import Parsers
import Data.Maybe
import Data.Map.Strict as M hiding (foldl, map)

pt8 :: IO()
pt8 = do
    input <- linesWords 8
    timed $ ptA input
    timed $ ptB input
    
pt8test :: IO()
pt8test = do
    input' <- readFile "test.1"
    let input =  map words (lines input')
    timed $ ptA input

ptA :: [[String]] -> Int
ptA input = maximum $ elems $ fst $ foldl parse (empty, 0) input

ptB :: [[String]] -> Int
ptB input = snd $ foldl parse (empty, 0) input

parse :: (Map String Int, Int) -> [String] -> (Map String Int, Int)
parse (m, maxV) x = if ope (getVal m ifKey) ifVal then (insert key newVal m, max newVal maxV) else (m, maxV)
    where
        key = head x
        func = if (x!!1) == "dec" then (-) else (+)
        val = read (x!!2)::Int
        ifKey = x!!4
        ope = makeOpe (x!!5)
        ifVal = read (x!!6)::Int
        newVal = func (getVal m key) val

makeOpe :: Ord a => String -> (a -> a -> Bool)
makeOpe "<" = (<)
makeOpe ">" = (>)
makeOpe "<=" = (<=)
makeOpe ">=" = (>=)
makeOpe "==" = (==)
makeOpe _ = (/=)

getVal :: Map String Int -> String -> Int
getVal m k = fromMaybe 0 (M.lookup k m)

