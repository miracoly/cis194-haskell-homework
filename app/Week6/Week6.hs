module Week6.Week6 where

-- Exercise 1
fib :: Integer -> Integer
fib n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fib1 :: [Integer]
fib1 = map fib [0..]

-- Exercise 2
-- Solution by https://github.com/bschwb/cis194-solutions/blob/main/06-lazy/Fibonacci.hs
fibs2 :: [Integer]
fibs2 = let fibo a b = a : fibo b (a+b) in fibo 0 1

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)