doubleList :: [Int] -> [Int]
doubleList xs = [2*a | a<-xs]

doubleListEven :: [Int] -> [Int]
doubleListEven xs = [2*a | a <- xs, isEven a]

sumPairs :: [(Int,Int)] -> [Int]
sumPairs lp = [a+b|(a,b)<-lp]

digits :: String -> String
digits st = [ch | ch<-st, isDigit ch]

isEven :: Int -> Bool
isEven a = (mod a 2 == 0)

isDigit :: Char -> Bool
isDigit a = (a>='0' && a<='9')

-------------------------------------------------------------------------------------------------------------------

new :: Char -> Int -> [(Char,Int)]
new a i = [(cs,is) | cs<-['a'..a], is<-[0..i]]

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)   --No terminal = applyTwice dobrar 5

dobrar :: Int -> Int
dobrar a = 2*a

-------------------------------------- REFAZENDO A PRIMEIRA AULA -------------------------------------------------------

vendas :: Int -> Int
vendas 0 = 30
vendas 1 = 40
vendas 2 = 60
vendas 3 = 100
vendas 4 = 90
vendas x = 0


total :: (Int->Int) -> Int -> Int
total f n
   | n==0         = f 0
   | otherwise    = f n + total f (n-1)

maxN :: Int -> Int -> Int
maxN a b | (a>b) = a
         | otherwise = b


maxFun :: (Int->Int) -> Int -> Int
maxFun f n 
    | n==0 = f 0
    | otherwise = maxN (maxFun f (n-1)) (f n)


-------------------------------------- CRESCENTE -------------------------------------------------------------------
isCrescent :: (Int->Int) -> Int -> Bool
isCrescent f n
    | n==0 = True
    | (f n <= f (n-1)) = False
    | (f n > f (n-1)) = isCrescent f (n-1)

list :: (Int-> Int) -> [Int] -> [Int]
list f [] = []
list f (a:as) = (f a) : list f as 

foldr1Quinhas :: (t->t->t) -> [t] -> t
foldr1Quinhas f [a] = a
foldr1Quinhas f (a:as) = f a (foldr1Quinhas f as) 

-- Algumas funções: map, foldr1, bool, concat, maximum, filter

foldrQuinhas :: (t->u->u) -> u -> [t] -> u
foldrQuinhas f s [] = s
foldrQuinhas f s (a:as) = f a (foldrQuinhas f s as)