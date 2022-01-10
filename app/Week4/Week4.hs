module Week4.Week4 where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- I could not come up with my own solution
foldTree :: [a] -> Tree a
foldTree _ = Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] 

sieveSundaram :: Integer -> [Integer]
sieveSundaram  = map (\x -> x * 2 + 1) . prePrime
    where prePrime n = takeWhile (\x -> x <= ((n - 1) `div` 2))
                        [1..n] \\ [i + j + 2 * i * j | j <- [1..n], i <- [1..n]]