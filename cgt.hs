--A game that is Zero (base case) is two empties
--Anything else must be two lists of games, a left list and a right list.

data Game = Position [Game] [Game]

--some basic games
zeroGame = Position [] []
starGame = putL zeroGame $ putR zeroGame zeroGame

--basic game functions
putL :: Game -> Game -> Game
putL goLeft (Position left right) = Position (goLeft:left) right

putR :: Game -> Game -> Game
putR goRight (Position left right) = Position left (goRight:right)

--overloading?
instance Show Game where
    show (Position [] []) = "{|}"
    show (Position left []) = "{"++(show (map show left))++"|}"
    show (Position [] right) ="{|"++(show (map show right))++"}"
    show (Position left right)
        = "{"++(show(map show left))++"|"++(show (map show right))++"}"

instance Num Game where
    negate (Position left right)
        = Position (map negate right) (map negate left)
    (Position xLeft xRight) + (Position yLeft yRight)
        = (Position ((map (+ (Position yLeft yRight)) xLeft)
        ++(map (+ (Position xLeft xRight)) yLeft))
        ((map (+ (Position yLeft yRight)) xRight)
            ++(map (+ (Position xLeft xRight)) yRight)))
    xGame - yGame = xGame + (negate yGame)
    fromInteger x
        | x > 0 = (Position [fromInteger (x-1)] [])
        | x < 0 = (Position [] [fromInteger (x+1)])
        | otherwise = (Position [] [])

--fun!