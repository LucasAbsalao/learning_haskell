printAnsXVezes :: Int -> String -> IO()
printAnsXVezes n s = putStr(printXVezes n s)

printXVezes :: Int -> String -> String
printXVezes 0 _ = ""
printXVezes n s = s ++ "\n" ++ printXVezes (n-1) s

sumSquares :: Int -> Int -> Int
sumSquares x y = sqX + sqY
    where sqX = x*x
          sqY = y*y

sumSquares2 :: Int -> Int -> Int
sumSquares2 x y = sq x + sq y
    where sq z= z*z

sumSquares3 :: Int -> Int -> Int
sumSquares3 x y = 
    let sqX = x*x
        sqY = y*y
    in sqX + sqY

sumn :: Int -> Int
sumn 0 = 0
sumn n = n + sumn (n-1)

isEven :: Int -> Bool
isEven n | (mod n 2) == 1 = False
         | otherwise = True

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)