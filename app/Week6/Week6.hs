module Week6.Week6 where

fib :: Integer -> Integer
fib n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fib1 :: [Integer]
fib1 = map fib [0..]

-- Solution by https://github.com/bschwb/cis194-solutions/blob/main/06-lazy/Fibonacci.hs
fibs2 :: [Integer]
fibs2 = let fibo a b = a : fibo b (a+b) in fibo 0 1
