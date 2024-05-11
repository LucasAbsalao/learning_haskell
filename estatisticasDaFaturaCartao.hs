stringToFloat :: String -> String
stringToFloat (a:as)
    | a==';' = ""
    | otherwise = a : stringToFloat as


getValue :: String -> Int -> [Float] 
getValue [] i = []
getValue (a:as) i
    | i==1 = [read (stringToFloat (a:as)) :: Float] ++ getValue as 0
    | a==';' = getValue as (i+1)
    | otherwise = getValue as i

--minMaxCartao :: String -> (Double, Double)