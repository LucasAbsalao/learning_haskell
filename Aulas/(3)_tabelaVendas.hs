addEspacos :: Int -> String
addEspacos 1 = " "
addEspacos x = " " ++ addEspacos (x-1)


paraDireita :: Int -> String -> String
paraDireita x a = addEspacos x ++ a


cabecalho::String
cabecalho = "Semanas" ++ paraDireita 4 "Vendas\n"


vendas :: Int -> Int
vendas 0 = 30
vendas 1 = 40
vendas 2 = 60
vendas 3 = 100
vendas 4 = 90
vendas x = 10


imprimeSemanas :: Int -> String
imprimeSemanas x
   | x==0     = paraDireita 3 (show(0)) ++ paraDireita 8 (show(vendas 0)) ++ "\n"
   | otherwise = imprimeSemanas (x-1) ++ paraDireita 3 (show(x)) ++ paraDireita 8 (show(vendas x)) ++ "\n"
--Ajuste
--where ajuste = length(show(vendas n))


imprimeTotal :: Int -> String
imprimeTotal x = " Total" ++ paraDireita 6 (show(vendasTotal x)) ++ "\n"


imprimeMedia :: Int -> String
imprimeMedia x = " Media" ++ paraDireita 6 (show(fromIntegral (vendasTotal x) / fromIntegral x)) ++ "\n"


vendasTotal :: Int -> Int
vendasTotal x
   | (x==0) = vendas 0
   | otherwise = vendas x + vendasTotal (x-1) 


imprimeQuebraLinha :: String
imprimeQuebraLinha = "\n"


imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(imprimeQuebraLinha
               ++ cabecalho
               ++ imprimeSemanas n
               ++ imprimeQuebraLinha
               ++ imprimeTotal n
               ++ imprimeMedia n
               ++ imprimeQuebraLinha)
