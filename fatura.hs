main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result

getString :: String -> String
getString (a:as)
    | a==';' = ""
    | otherwise = a : getString as

getMonth :: String -> String
getMonth (' ':as) = getString as
getMonth (a:as) = getMonth as

getValue :: String -> Int -> Double 
getValue [] i = 0
getValue (a:as) i
    | i==3 && a/=';' = read (getString (a:as)) :: Double
    | a==';' = getValue as (i+1)
    | otherwise = getValue as i

nextMonth :: String -> Int -> String
nextMonth "" i = ""
nextMonth (a:as) i
    | i==4 && a/=';' = as
    | a==';' = nextMonth as (i+1)
    | otherwise = nextMonth as i

logMesLista :: String -> String -> [Double]
logMesLista month [] = []
logMesLista month fat
    | getMonth fat == month = [(getValue fat 1)] ++ (logMesLista month (nextMonth fat 1))
    | otherwise =  logMesLista month (nextMonth fat 1)
    
logMes :: String -> String -> Double
logMes month fatura = sum(logMesLista month fatura)

-- logMesDouble :: String -> String -> Double
-- logMesDouble month [] = 0
-- logMesDouble month fat
--     | getMonth fat == month = (getValue fat 1) + (logMesDouble month (nextMonth fat 1))
--     | otherwise =  logMesDouble month (nextMonth fat 1)
    
-- final :: [Double] -> Double
-- final (a:as) 
--     |as==[] = a
--     |otherwise = a+final as 