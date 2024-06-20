
--Segunda questão
merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] (b:bs) = [b] ++ merge [] bs
merge (a:as) [] = [a] ++ merge as []
merge (a:as) (b:bs)
    |a<=b = [a] ++ merge as (b:bs)
    |otherwise = [b] ++ merge (a:as) bs

--Terceira questão
type Pilha t = [t]
data Elemento = Valor Int | Soma | Multiplica deriving (Show)

exemploPilhaElem :: Pilha Elemento
exemploPilhaElem = [Valor 10, Valor 20, Soma, Valor 30,Valor 40, Multiplica]

gera_string :: Pilha Elemento -> String
gera_string pilha = aux pilha ""

aux :: Pilha Elemento-> String -> String
aux [] s = s
aux ((Valor a):(Valor b):Soma:resto) s = aux resto (expr ((Valor a):(Valor b):Soma:resto) s)
aux ((Valor a):(Valor b):Multiplica:resto) s = aux resto (expr ((Valor a):(Valor b):Multiplica:resto) s)
aux ((Valor a):Soma:resto) s = aux resto (expr ((Valor a):Soma:resto) s)
aux ((Valor a):Multiplica:resto) s = aux resto (expr ((Valor a):Multiplica:resto) s)

expr :: Pilha Elemento-> String -> String --jeito burro
expr ((Valor a):(Valor b):Soma:resto) s ="(" ++ s ++ show(a) ++ "+" ++ show(b)++")"
expr ((Valor a):(Valor b):Multiplica:resto) s ="(" ++ s ++ show(a) ++ "*" ++ show(b)++")"
expr ((Valor a):Soma:resto) s ="(" ++ s ++ "+" ++ show(a)++")"
expr ((Valor a):Multiplica:resto) s ="(" ++ s ++ "*" ++ show(a)++")"

-- exemplo de uso: gera_string exemploPilhaElem ——> "((10+20)*30)"
