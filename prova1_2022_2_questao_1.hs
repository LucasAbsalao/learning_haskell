rlencode0 :: [Int] -> [Int]
rlencode0 a = search0 a 0 False

search0 :: [Int] -> Int -> Bool -> [Int]
search0 [] n b = []
search0 (a:as) n cond
    | a==0 = search0 as (n+1) True
    | a/=0 && cond==True = [0] ++ [n] ++ [a] ++ search0 as 0 False
    | as == [] && cond==True = [0] ++ [n+1] ++ search0 as n False
    | otherwise = [a] ++ search0 as 0 False

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 (0:b:as) = add0 b ++ rldecode0 as
rldecode0 (a:as) = [a] ++ rldecode0 as

add0 :: Int -> [Int]
add0 0 = []
add0 a = [0] ++ add0 (a-1)