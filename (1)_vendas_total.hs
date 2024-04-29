main :: IO ()
main = putStrLn (show(maxVendas 4))


square :: Int -> Int
square x = x * x


allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n==m) && (m==p)


maxi :: Int -> Int -> Int
maxi m n | n >= m     = n
        | otherwise  = m 


addD :: Int -> Int -> Int
addD a b = 2 * (a+b)


vendas :: Int -> Int
vendas 0 = 30
vendas 1 = 40
vendas 2 = 60
vendas 3 = 100
vendas 4 = 90
vendas x = 0


vendasTotal :: Int -> Int
vendasTotal n
   | n==0         = vendas 0
   | otherwise    = vendas n + vendasTotal (n-1)


fatorial :: Integer -> Integer
fatorial n
   | n==0     = 1
   |otherwise = n * fatorial (n-1)


maxVendas :: Int -> Int
maxVendas n
   | n==0     = vendas 0
   | otherwise = maxi (maxVendas(n-1)) (vendas n)




myNot :: Bool -> Bool
myNot True = False
myNot False = True


myOr :: Bool -> Bool -> Bool
myOr True x = True
myOr False x = x


myAnd :: Bool -> Bool -> Bool
myAnd False x = False
myAnd True x = x


-- maxiThree :: Int -> Int -> Int -> Int
-- maxiThree m n p
--     | m>=n>=p = m
--     | n>m>p = n
--     | p>m>n = p




equal :: Int -> Int -> Int
equal m n
   | m==n =1
   |otherwise =0       


equalCount :: Int -> Int -> Int -> Int -> Int
equalCount mx m n p = equal mx m + equal mx n + equal mx p