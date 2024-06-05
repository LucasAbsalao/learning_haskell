data Comando = ParaFrente Int | ParaTras Int | Escreva Char

posicaofinal :: String -> [Comando] -> Int
posicaofinal s [] = 1
posicaofinal s (ParaFrente x : as) = x + posicaofinal s as
posicaofinal s (ParaTras x : as) = -x + posicaofinal s as
posicaofinal s (a:as) = posicaofinal s as

estadofinal :: String -> [Comando] -> String
estadofinal s comands = estadofinaln s comands 1

estadofinaln :: String -> [Comando] -> Int -> String
estadofinaln s [] i = s
estadofinaln s (ParaFrente n : as) i = estadofinaln s as (i+n)
estadofinaln s (ParaTras n : as) i = estadofinaln s as (i-n)
estadofinaln s (Escreva c : as) i = estadofinaln (change s 1 i c) as i

change :: String -> Int -> Int -> Char -> String
change [] _ _ _= []
change (a:as) i limite c 
    | i==limite = c : change as (i+1) limite c
    | otherwise = a : change as (i+1) limite c

interprete :: String -> [Comando] -> Char
interprete s lista = interpreten (estadofinal s lista) 1 (posicaofinal s lista)

interpreten :: String -> Int -> Int -> Char
interpreten (a:as) i lim
    | i==lim = a
    | otherwise = interpreten as (i+1) lim
