-- stringLists :: String -> String -> String -> [String]
-- stringLists [] [] actual = []
-- stringLists (a:[]) str2 actual= [actual++[a]] ++ stringLists str2 [] ""
-- stringLists (a:as) str2 actual
--     | a==' ' = [actual] ++ stringLists as str2 ""
--     | a>='A' && a<='Z' = stringLists as str2 (actual++[upperToLower a])
--     | otherwise = stringLists as str2 (actual++[a])

stringLists :: String -> String -> [String]
stringLists "" "" = []
stringLists "" str2 = stringLists str2 ""
stringLists str1 str2 = [spanEspacos str1 str2] ++ stringLists (nextWord str1 False) str2

nextWord :: String -> Bool -> String
nextWord "" b = ""
nextWord (' ':as) b = nextWord as True
nextWord (a:as) True = (a:as) 
nextWord (a:as) b = nextWord as b

spanEspacos :: String -> String -> String
spanEspacos "" str2 = "" 
spanEspacos (' ':as) str2= "" 
spanEspacos (a:as) str2
    | a>='A' && a<='Z' = (upperToLower a) : spanEspacos as str2
    | otherwise = a : spanEspacos as str2

upperToLower :: Char -> Char
upperToLower a = toEnum (fromEnum a + fromEnum ' ')

isUnique :: [String] -> String -> Bool
isUnique [] search = True
isUnique (a:as) search
    | a==search = False
    | otherwise = isUnique as search

removeWord :: [String] -> String -> [String]
removeWord [] search = []
removeWord (a:as) search
    | a==search = removeWord as search
    | otherwise = a : removeWord as search

uncommonWords :: [String] -> [String]
uncommonWords [] = [] 
uncommonWords (a:as)
    | isUnique as a = a : uncommonWords as
    | otherwise = uncommonWords (removeWord as a)

uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences str1 str2 = qsort(uncommonWords (stringLists str1 str2))

qsort :: [String] -> [String]
qsort [] = []
qsort (a:as) = qsort [n | n <- as, n<a] ++ [a] ++ qsort [n | n <- as, n>=a]

main = do
    sentence_1 <- getLine
    sentence_2 <- getLine
    let result = uncommonFromTwoSentences sentence_1 sentence_2
    print result