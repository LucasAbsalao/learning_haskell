type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa lista = executaComandos lista 0

executaComandos :: [(Comando, Valor)] -> Int -> Int
executaComandos [] a = a
executaComandos (("Soma", n):as) a = executaComandos as (a+n)
executaComandos (("Subtrai", n):as) a = executaComandos as (a-n)
executaComandos (("Multiplica", n):as) a = executaComandos as (a*n)
executaComandos (("Divide", 0):as) a = executaComandos as (-666)
executaComandos (("Divide", n):as) a = executaComandos as (div a n)

main = do
    a <- getLine
    let result = executa (read a)
    print result