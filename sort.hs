maxInt :: [Int] -> Int
maxInt (a:[]) = a
maxInt (a:as)
    |a>maxInt(as) = a
    |otherwise = maxInt as

remove :: [Int] -> Int -> Bool -> [Int]
remove [] b c = []
remove (a:as) b c
    |a==b && c==True = remove as b False
    |otherwise = [a] ++ remove as b c

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar a = ordenar(remove a (maxInt a) True) ++ [maxInt a]

--bSort :: [String] -> [String]
