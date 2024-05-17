data Shape =
   Circle Float
   | Rectangle Float Float


maxi :: Int -> Int -> Int
maxi a b
   |a>b = a
   |otherwise = b


double :: Int -> Int
double a = 2*a


-- Circle 4.9 a :: Shape
-- Rectangle 4.2 2.0 b :: Shape


isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False


data Expr =
   Lit Int |
   Add Expr Expr|
   Sub Expr Expr


eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


showExpr :: Expr -> String
showExpr (Lit n) = show(n)
showExpr (Add e1 e2) = "("++showExpr(e1)++" + " ++ showExpr(e2) ++ ")"
showExpr (Sub e1 e2) = "("++showExpr(e1)++" - " ++ showExpr(e2) ++ ")"


data ListaEncadeada t = ListaVazia | CabecaECauda t (ListaEncadeada t)
   deriving Show
exemploLista :: ListaEncadeada Int
exemploLista = CabecaECauda 4 (CabecaECauda 5 ListaVazia)




data Lista t = Vazio | Cons t (Lista t)
--exemploLista2 :: Lista Int
exemploLista2 = Cons 4 (Cons 5 Vazio)


showLista :: Show t => Lista t -> String
showLista Vazio = ""
showLista (Cons elemento lista) = show elemento++","++showLista lista


toList :: Lista t -> [t]
toList Vazio = []
toList (Cons elemento lista) = [elemento]++toList lista


fromList :: [t] -> Lista t
fromList [] = Vazio
fromList (a:as) = Cons a (fromList as)


data Tree t = NilT | Node t (Tree t) (Tree t)
exemploTree = Node 4 (Node 5 (Node 6 NilT (Node 7 (Node 10 NilT NilT) NilT)) NilT) (Node 8 (Node 9 NilT NilT) NilT)


depth :: Tree t -> Int
depth NilT = 0
depth (Node a tree1 tree2) = 1 + (maxi (depth tree1) (depth tree2))


collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node a tree1 tree2) = collapse tree1 ++ [a] ++ collapse tree2


mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node a tree1 tree2) = (Node (f a) (mapTree f tree1) (mapTree f tree2))
