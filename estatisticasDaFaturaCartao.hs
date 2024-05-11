main = do
    a <- getLine
    let result = minMaxCartao a
    print result

    
stringToFloat :: String -> String
stringToFloat (a:as)
    | a==';' = ""
    | otherwise = a : stringToFloat as


getValue :: String -> Int -> [Double] 
getValue [] i = []
getValue (a:as) i
    | i==3 = [read (stringToFloat (a:as)) :: Double] ++ getValue as 0
    | a==';' = getValue as (i+1)
    | otherwise = getValue as i

maxFloat :: [Double] -> Double
maxFloat (a:[]) = a
maxFloat (a:as) =  max a (maxFloat as)

minFloat :: [Double] -> Double
minFloat (a:[]) = a
minFloat (a:as) =  min a (minFloat as)

minMaxCartao :: String -> (Double, Double)
minMaxCartao a = (minFloat (getValue a 1), maxFloat (getValue a 1))