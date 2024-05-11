dividers :: Int -> Int -> Int
dividers n i
    |n==0 = 1
    |n==i = 1
    |mod n i == 0 = 1 + dividers n (i+1) 
    |otherwise = dividers n (i+1) 


checkPrime :: Int -> Bool
checkPrime a = (dividers a 1 == 2)

firstDivider :: Int -> Int -> Int
firstDivider n i
    | mod n i == 0 = i
    | otherwise = firstDivider n (i+1)

manyDividers :: Int -> Int -> Int
manyDividers n x
    |mod n x == 0 = 1 + manyDividers (div n x) x
    |otherwise = 0

fatPrime :: Int -> [(Int, Int)]
fatPrime 1 = []
fatPrime a = [(firstDivider a 2, manyDividers a (firstDivider a 2))] ++ fatPrime (div a ((firstDivider a 2) ^ (manyDividers a (firstDivider a 2))))
