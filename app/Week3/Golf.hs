module Week3.Golf where
import Data.Maybe
import Data.List (last, group, sort)

histogram :: [Int] -> String
histogram [] = "==========\n0123456789\n"
histogram ls = unlines $ lines ls ++ [histogram []]
    where
        lines :: [Int] -> [String]
        lines = map drawLine . reverse . extractLines . groupNumbers


drawLine :: [Int] -> String
drawLine = drawAsterisk "          "
    where
        drawAsterisk :: String -> [Int] -> String
        drawAsterisk str [] = str
        drawAsterisk str (x:xs) =
            let (ys, _:zs) = splitAt x str in
                drawAsterisk (ys ++ "*" ++ zs) xs

groupNumbers :: [Int] -> [[Int]]
groupNumbers = group . sort

extractLines :: [[Int]] -> [[Int]]
extractLines [[]] = []
extractLines ls = [x | (x:_) <- ls] : extractLines (dropFirst ls)

dropFirst :: [[Int]] -> [[Int]]
dropFirst [] = []
dropFirst ([]:ys) = dropFirst ys
dropFirst ((x:xs):ys) = xs : dropFirst ys


-- [1,1,1,5]  -> [[1],[1],[1, 5]]
-- [1,4,5,4,6,6,3,4,2,4,9] -> [[4],[4],[4,6],[1,2,3,4,5,6,9]]
-- [1,2,3,4,4,4,4,5,6,6,9] -> [[1],[2],[3],[4,4,4,4],[5],[6,6],[9]]
--     -> [1,2,3,4,5,6,9] ++ [[4,4,4],[6]]
--            "            -> [1,1,1,4,1,2,0,0,1]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

skips :: [a] -> [[a]]
skips = skips' 1
    where
        skips' :: Int -> [a] -> [[a]]
        skips' n xs
            | n > length xs = []
            | otherwise = takeEveryNth n xs : skips' (n + 1) xs

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n xs
    | n < 1 || n > length xs = []
    | otherwise = (xs !! (n - 1)) : takeEveryNth n (drop n xs)