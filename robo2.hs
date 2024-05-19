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

faces :: Direction -> [Command] -> Direction
faces d [] = d
faces d (TurnLeft:as)= faces (turn d) as
faces d (TurnRight:as) = faces (turn (reversei d)) as
faces d ((Forward n):as) = faces d as
faces d ((Backward n):as) = faces d as



main = do
    a <- getLine
    b <- getLine
    let result = faces (read a) (read b)
    print result