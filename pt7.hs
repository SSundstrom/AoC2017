import Data.Map.Strict as M (Map, insert, (!), fromList)
import Data.Maybe
import Data.List hiding ((\\))
import Text.Regex
import Parsers
import Data.Set as S (insert, (\\), empty, toList)


pt7 :: IO ()
pt7 = do
    input <- linesWords 7
    timed (ptA input)
    timed (ptB input)
    day07


pt7test :: IO ()
pt7test = do
    input <- readFile "test"
    let input' = map words (lines input) 
    -- timed (ptA input')
    putStrLn $ ptB input'

ptA :: [[String]] -> [String]
ptA input = toList $ parent \\ children
        where 
            cleaned = map (concat . mapMaybe (matchRegex rgexPtA)) input
            parent = foldr (S.insert . head) empty cleaned
            children = foldr S.insert empty (concatMap tail cleaned)

rgexPtA = mkRegex "([a-z]+)"

ptB :: [[String]] -> String
ptB input = do
    let x = head $ ptA input
    let ogMap = makeMap $ parse' input
    let fixedMap = fixScore ogMap x
    let odd = findOdd fixedMap x
    let diff' = nub $ map (fst . (!) fixedMap) $ snd $ fixedMap ! x
    let diff = maximum diff' - minimum diff'
    show $ fst (ogMap ! odd) - diff

rgex = mkRegex "([a-z]+|[0-9]+)"

type DiscMap = Map String (Int, [String])

makeMap :: [[String]] -> DiscMap
makeMap list = fromList $ map (\x -> (head x, (read (x!!1)::Int, drop 2 x))) list

parse' :: [[String]] -> [[String]]
parse' = map (concat . mapMaybe (matchRegex rgex))

fixScore :: DiscMap -> String -> DiscMap
fixScore ogm p = M.insert p (foldr (\x y -> y + fst (newm ! x)) size child, child) newm
    where
        (size, child) = ogm ! p
        newm = foldl fixScore ogm child

findOdd :: DiscMap -> String -> String
findOdd map' parent = if null uChild then parent else findOdd map' (fst (head uChild))
    where
        childStr = snd (map' ! parent)
        childTup = zip childStr (map (fst . (!) map' ) childStr)
        uChild = unique childTup

unique :: Ord a => [(b,a)] -> [(b,a)]
unique list = filter (\x -> length (filter (snd x==) (map snd list)) < 2) list