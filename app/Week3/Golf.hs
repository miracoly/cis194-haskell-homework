module Week3.Golf where
import Data.Maybe
import Data.List (last)

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > z && y > z = y : localMaxima (y:z:xs)
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