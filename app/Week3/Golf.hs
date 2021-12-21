module Week3.Golf where
import Data.Maybe
import Data.List (last)

-- skips :: [a] -> [[a]]
-- skips [] = []
-- skips xs = [xs] ++ 


takeEveryNth :: Int -> [a] -> [a]
takeEveryNth _ [] = []
takeEveryNth n xs
    | n < 1 = []
    | n > length xs = []
    | otherwise = (xs !! (n - 1)) : takeEveryNth n (drop n xs)

-- safeLast :: [a] -> [a]
-- safeLast xs
--     | isNothing (Safe.lastMay xs) = []
--     | otherwise = fromJust $ Safe.lastMay xs