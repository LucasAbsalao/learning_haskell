main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result

maxString :: [String] -> String
maxString (a:[]) = a
maxString (a:as)
    |a>maxString(as) = a
    |otherwise = maxString as

remove :: [String] -> String -> Bool -> [String]
remove [] b c = []
remove (a:as) b c
    |a==b && c==True = remove as b False
    |otherwise = [a] ++ remove as b c

bSort :: [String] -> [String]
bSort [] = []
bSort a = bSort(remove a (maxString a) True) ++ [maxString a]

--bSort :: [String] -> [String]

quickSort :: [String] -> [String]
