formDuos :: [String] -> [(String, Int)]
formDuos [] = []
formDuos (a:as) = (a,contarPalavra (a:as) a) : formDuos (removePalavra as a)

removePalavra :: [String] -> String -> [String]
removePalavra [] search = []
removePalavra (a:as) search
    | a==search = removePalavra as search
    | otherwise = a:removePalavra as search

contarPalavra :: [String] -> String -> Int
contarPalavra [] search = 0
contarPalavra (a:as) search
    | a==search = 1+contarPalavra as search
    | otherwise = contarPalavra as search 

maxString :: [(String, Int)] -> (String,Int)
maxString [] = ("",0)
maxString [duo] = duo
maxString ((a,b):as)
    | b > snd (maxString as) = (a,b)
    | b == snd (maxString as) && length a < length(fst (maxString as)) = (a,b)
    | otherwise = maxString as

removeDuo :: [(String,Int)] -> (String,Int) -> [(String,Int)]
removeDuo [] search = []
removeDuo (a:as) search
    | a==search = removeDuo as search
    | otherwise = a:removeDuo as search

taken :: [(String,Int)] -> Int -> [String]
taken [] n = []
taken lista 0 = []
taken lista n = fst (maxString lista) : taken (removeDuo lista (maxString lista)) (n-1)

palavrasFrequentes :: [String] -> [String]
palavrasFrequentes lista = taken (formDuos lista) 3

main = do
        lista <- getLine
        print $ palavrasFrequentes (read lista :: [String])