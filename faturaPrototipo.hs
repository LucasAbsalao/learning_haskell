    -- main = do
    --     a <- getLine
    --     b <- getLine
    --     let result = logMesDouble a b
    --     print result

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
        | i==3 = (read (getString (a:as)))
        | a==';' = getValue as (i+1)
        | otherwise = getValue as i

    nextMonth :: String -> Int -> String
    nextMonth "" i = ""
    nextMonth (a:as) i
        | i==4 = as
        | a==';' = nextMonth as (i+1)
        | otherwise = nextMonth as i

    logMesDouble :: String -> String -> Double -> Double
    logMesDouble month "" d = d
    logMesDouble month fat d
        | getMonth fat == month = logMesDouble month (nextMonth fat 1) (d+getValue fat 1) --Arredondar para 2 casas decimais ficaria = roundValue ((getValue fat 1) + (logMesDouble month (nextMonth fat 1)))
        | otherwise =  logMesDouble month (nextMonth fat 1) d

    logMesLista :: String -> String -> [Double]
    logMesLista month [] = []
    logMesLista month fat
        | getMonth fat == month = [(getValue fat 1)] ++ (logMesLista month (nextMonth fat 1))
        | otherwise =  logMesLista month (nextMonth fat 1)
        
    final :: [Double] -> Double
    final [] = 0
    final (a:as) = a+final as 
        
    roundValue :: Double -> Double
    roundValue 0 = 0
    roundValue x = fromIntegral ( dif (x*100) (intAbove (x*100) 0) ) / 100

    intAbove :: Double -> Int -> Int
    intAbove d i
        | (fromIntegral i) >d = i
        | otherwise = intAbove d (i+1)

    dif :: Double -> Int -> Int
    dif d i
        | dif1 > dif2 = i-1
        | otherwise = i
        where 
            dif1 = fromIntegral i - d
            dif2 = d - fromIntegral (i-1)
