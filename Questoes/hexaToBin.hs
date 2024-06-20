pot2 :: Int -> Int -> String
pot2 a 0 = ""
pot2 a p = show(div a p) ++ pot2 (mod a p) (div p 2)

letterToNumber :: Char -> Int
letterToNumber a
    | a<='9' && a>='0' = fromEnum a - fromEnum '0'
    | a=='A' = 10
    | a=='B' = 11
    | a=='C' = 12
    | a=='D' = 13
    | a=='E' = 14
    | a=='F' = 15

htob :: String -> String
htob "" = ""
htob (a:as) = pot2 (letterToNumber a) 8 ++ htob as

main = do
    s <- getLine
    let result = htob s
    print result