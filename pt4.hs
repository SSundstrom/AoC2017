import Parsers

pt1 :: IO ()
pt1 = do
    ms <- linesWords 4
    print $ show $ sum $ map (valid) ms

valid :: [String] -> Int
valid [] = 1
valid (x:xs) = if (not (elem x xs) && 1 == valid xs) then 1 else 0


pt2 :: IO ()
pt2 = do
    ms <- linesWords 4
    print $ show $ sum $ map (anaValid) ms

test = ["abcde","xyz","ecdab"] --Not vaild
test2 = [ "iiii", "oiii", "ooii", "oooi", "oooo"] --Valid
test3 = ["oiii", "ioii", "iioi", "iiio"] -- Not Valid
test4 = ["a", "ab" ,"abc" ,"abd" ,"abf" ,"abj"] --Valid

anaValid :: [String] -> Int
anaValid [] = 1
anaValid (x:xs) = if not (any (isAna x) xs) && anaValid xs == 1 then 1 else 0

isAna :: String -> String -> Bool
isAna [] [] = True
isAna (x:xs) y
    | length (x:xs) == length y = if elem x y then isAna xs (delFirstOcc x y) else False
    | otherwise = False

delFirstOcc :: Char -> String -> String
delFirstOcc c (x:xs)
    | c == x = xs
    | otherwise = x : delFirstOcc c xs