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
    | i==3 = read (getString (a:as)) :: Double
    | a==';' = getValue as (i+1)
    | otherwise = getValue as i

nextMonth :: String -> Int -> String
nextMonth "" i = ""
nextMonth (a:as) i
    | i==4 = as
    | a==';' = nextMonth as (i+1)
    | otherwise = nextMonth as i

logMes :: String -> String -> Double
logMes month [] = 0
logMes month fat
    | getMonth fat == month = roundValue ((getValue fat 1) + (logMes month (nextMonth fat 1)))
    | otherwise =  logMes month (nextMonth fat 1)

roundValue :: Double -> Double
roundValue x = fromIntegral (round (x*100)) / 100