data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
    deriving (Eq, Show, Read)

data Direction = North | South | West | East
    deriving (Read, Show)


turn :: Direction -> Direction
turn North = West
turn West = South
turn South = East
turn East = North

reversei :: Direction -> Direction
reversei North = South
reversei South = North
reversei East = West
reversei West = East

faces :: Direction -> [Command] -> [Direction]
faces d [] = [d]
faces d (a:as)
    | a==TurnLeft = [d]++faces (turn d) as
    | a==TurnRight = [d]++faces (turn (reversei d)) as
    | a==(Forward n) = [d]++faces d as
    | a==(Backward n) = [d]++faces (reversei d) as


main = do
    a <- getLine
    b <- getLine
    let result = faces (read a) (read b)
    print result