data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

intToChar :: Int -> Char
intToChar 0 = 'E'
intToChar 1 = 'M'
intToChar 2 = 'A'
intToChar 3 = 'C'
intToChar 4 = 'S'

divide8 :: String -> Int -> String -> [String]
divide8 (a:[]) i eachone = [eachone++a]
divide8 (a:as) i eachone
    | i==7 = [eachone++[a]] ++ divide8 as 0 ""
    | otherwise = divide8 as (i+1) (eachone++[a])

linearList :: Tree Int -> String
linearList Nilt = ""
linearList (Node a tree1 tree2) = linearList tree1 ++ [intToChar (mod a 5)] ++ linearList tree2

dna1 :: Tree Int -> [String]
dna1 Nilt = []
dna1 tree1 = divide8 (linearList tree1) 0 ""

main :: IO ()
main = do
  input <- getLine
  let result = dna1 (read input :: Tree Int)
  print result

