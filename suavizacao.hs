main = do
        lista <- getLine
        print $ suaviza (read lista :: [Float])

suaviza :: [Float] -> [Float]
suaviza [] = []
suaviza lista = suavizaCont lista True

suavizaCont :: [Float] -> Bool -> [Float]
suavizaCont [] cond = []
suavizaCont (a:as) True = [a] ++ suavizaCont (a:as) False
suavizaCont (a:[]) False = [a]
suavizaCont (a:b:[]) False = [b]
suavizaCont (a:b:c:as) False = [newB] ++ suavizaCont (b:c:as) False
    where newB = (a+b+c)/3



-- suaviza [] = []
-- suaviza (ap:a:[]) = [ap]
-- suaviza (ap:a:b:[]) = [ap] ++ (suaviza (a:b:[]))
-- suaviza (ap:a:b:c:resto) = [ap] ++ (suaviza (newB:a:b:c:resto))
--     where newB = (a+b+c)/3
-- -- suaviza(a:[]) = [a] ++ suaviza []
-- -- suaviza (a:)
-- -- suaviza (a:b:c:as)