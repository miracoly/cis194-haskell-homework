module Week3.Golf where
import Data.Maybe
import Data.List

histogram :: [Int] -> String
histogram ls = concat $ lines ls ++ ["==========\n0123456789\n"]
    where
        lines :: [Int] -> [String]
        lines = map drawLine . reverse . extractLines . group . sort

drawLine :: [Int] -> String
drawLine = drawAsterisk "          "
    where
        drawAsterisk :: String -> [Int] -> String
        drawAsterisk str [] = str ++ "\n"
        drawAsterisk str (x:xs) =
            let (ys, _:zs) = splitAt x str in
                drawAsterisk (ys ++ "*" ++ zs) xs

extractLines :: [[a]] -> [[a]]
extractLines [] = []
extractLines ls = [x | (x:_) <- ls] : extractLines (dropFirst ls)

dropFirst :: [[a]] -> [[a]]
dropFirst = filter (not . null) . map dropFirst'
    where
        dropFirst' :: [a] -> [a]
        dropFirst' [] = []
        dropFirst' [x] = []
        dropFirst' xs = tail xs

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