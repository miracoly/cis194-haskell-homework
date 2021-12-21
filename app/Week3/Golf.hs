module Week3.Golf where
import Data.Maybe
import Data.List (last)

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