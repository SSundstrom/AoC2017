-- Varje varv innehåller x * y siffor. med x*x som värdet nere till höger och (x-1)*(x-1)+1 rakt höger.
import Data.Time
import System.Environment


findSteps :: Int -> Int
findSteps x = div lap 2 + distanceToMiddle x lap
    where lap = findLap x

findLap :: Int -> Int
findLap x = findLap' x 1

findLap' :: Int -> Int -> Int
findLap' x y = if x > (y^2) then findLap' x (y+2) else (y)


distanceToMiddle :: Int -> Int -> Int
distanceToMiddle x lap = abs(p - div lap 2)
    where p = findOffset x lap

findOffset :: Int -> Int -> Int
findOffset x lap = rem (rem x ((lap - 2) ^ 2)) (lap-1)

type Pos = (Int, Int)

a = make1Bigger [[1]]


pt2 :: IO ()
pt2 = do
    let val = 277678
    start <- getCurrentTime
    let ans = buildUntil val (2, 1) (make1Bigger [[1]])
    end <- getCurrentTime
    print $ show ans 
    print $ diffUTCTime end start    
    -- putStrLn $ show res

buildUntil :: Int -> Pos -> [[Int]] -> Int
buildUntil v p m
    | v < nextV = nextV
    | p == (size+1, size) = buildUntil v (x+1, y) ((make1Bigger m) !!= ((x+1,y+1), nextV))
    | y == size = buildUntil v (x+1, y) (m !!= (p, nextV))
    | x == 0 = buildUntil v (x, y+1) (m !!= (p, nextV))
    | y == 0 = buildUntil v (x-1, y) (m !!= (p, nextV))
    | x == size = buildUntil v (x, y-1) (m !!= (p, nextV))
    | otherwise = -1
    where 
        size = (length m -1)
        (x,y) = p
        nextV = calcV p m

expand :: [[Int]] -> Pos -> [[Int]]
expand m p = ((make1Bigger m) !!= ((x+1,y+1), calcV p m))
        where (x,y) = p

make1Bigger :: [[Int]] -> [[Int]]
make1Bigger old = concat [[newLine], (map (\x -> concat [[0], x, [0]]) old), [newLine]]
    where
        newLine = [0 | x <- [0..(1+length old)]]

(!!=) :: [[Int]] -> (Pos, Int) -> [[Int]]
(!!=) m ((x,y), v) = pre ++ (concat [take x row, [v], drop (x+1) row] : end)
    where 
        pre = (take y m)
        end = (drop (y+1) m)
        row = (drop y m)!!0

calcV :: Pos -> [[Int]] -> Int
calcV p m
    | (size+1, size) == p = sum [relV (-1, 0), relV (-1, -1)]
    | (0, 0) == p = sum [relV (1, 0), relV (1,1)]
    | (0, size) == p = sum [relV (0, -1), relV (1, -1)]
    | (size, 0) == p = sum [relV (0, 1), relV (-1, 1)]
    | (size, size) == p = sum [relV (-1, 0), relV (-1, -1), relV (0, -1)]
    | y == size = sum [relV (-1, 0), relV (-1, -1), relV (0, -1), relV (1, -1)]
    | x == 0 = sum [relV (0, -1), relV (1, -1), relV (1, 0), relV (1, 1)]
    | y == 0 = sum [relV (1, 0), relV (-1, 1), relV (0, 1), relV (1, 1)]
    | x == size = sum [relV (0, 1), relV (-1, -1), relV (-1, 0), relV (-1, 1)]
    | otherwise = -1
    where 
        (x,y) = p
        size = (length m)-1
        relV (rx, ry) = m !! (y+ry) !! (x+rx)
        