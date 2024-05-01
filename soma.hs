soma :: [Int] -> Int
soma [] = 0
soma (0:as) = 0
soma (a:as) = a + soma as

nextList :: [Int] -> [Int]
nextList [] = []
nextList(0:as) = (0:as)
nextList(a:as) = nextList as

maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar (0:0:as) = []
maquinaSomar (0:as) 
    | as==[] = [] ++ maquinaSomar as
    | otherwise = [soma as] ++ maquinaSomar (nextList as)
maquinaSomar (a:as) = [soma (a:as)] ++ maquinaSomar (nextList as)

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])

-- maquinaSomar (0:as) 
--     | as==[] = [] ++ maquinaSomar as
--     | otherwise = [soma as] ++ maquinaSomar as
-- maquinaSomar (a:as) = maquinaSomar as