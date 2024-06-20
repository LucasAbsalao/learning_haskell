fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)


fibonacci :: [Int]
fibonacci = map fib [1..]




primes :: [Int]
primes = retira [2..]


retira :: [Int] -> [Int]
retira [] = []
retira (a:as) = [a] ++ [x | x <- (retira as), (mod x a /= 0)] -- (/y -> y `mod` a /=0 )