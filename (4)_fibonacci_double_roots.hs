intP :: (Int,Int)
intP = (33,43)


--(True, 'x') :: (Bool,Char)
--(34,22,'b') :: (Int,Int,Char)


addPair :: (Int,Int) -> Int
addPair (x,y) = x+y


shift :: (Int, (Int,Int)) -> ((Int,Int), Int)
shift (a,(b,c)) = ((a,b), c)


type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Age, Phone)


namePerson :: Person -> Name
namePerson (n,a,p) = n


oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b/(2.0*a)


twoRoots :: Float -> Float -> Float -> (Float,Float)
twoRoots a b c = (d+e,d-e)
   where
   d = (-b)/(2.0*a)
   e = sqrt (b^2-4.0*a*c)/(2.0*a)


roots :: Float -> Float -> Float -> String
roots a b c
   | b^2==4.0*a*c = show(oneRoot a b c) ++ "\n"
   | b^2>4.0*a*c = show(twoRoots a b c) ++ "\n"
   | otherwise = "nao existe\n"


--LISTAS


vet :: Char -> Char -> Char -> [Char]
vet a b n = [a,b..n]


sumList :: [Int]->Int
sumList [] = 0
sumList (a:as) = a + sumList as


fibonacci :: Int -> [Int]
fibonacci n | (n == 0) = [1]
fibonacci n | (n == 1) = [1,1]
fibonacci n | otherwise = fibonacci (n-1) ++ [(fibonacci(n-1) !! (n - 1)) + (fibonacci(n-1) !! (n - 2))]


double :: [Int] -> [Int]
double [] = []
double (a:as) = [a*2] ++ double(as)


member :: [Int] -> Int -> Bool
member [] num = False
member (a:as) num
   |a==num = True
   |otherwise = member as num


digits :: String -> String
digits "" = ""
digits (a:as)
   |a>='0'&&a<='9' = [a] ++ digits(as)
   |otherwise = digits(as)


sumListPair :: [(Int,Int)]->[Int]
sumListPair [] = []
sumListPair ((a,b):as) = [a+b] ++ sumListPair as


fibohead :: Int -> [Int]
fibohead 0 = [1]
fibohead 1 = [1,1]
fibohead (a:as) = [a] : fibohead(head (as))
