--A game that is Zero (base case) is two empties
--Anything else must be two lists of games, a left list and a right list.

data Game = Position [Game] [Game]

zeroGame = Position [] []

putL :: Game -> Game -> Game
putL goLeft (Position left right) = Position (goLeft:left) right

putR :: Game -> Game -> Game
putR goRight (Position left right) = Position left (goRight:right)

instance Show Game where
    show (Position [] []) = "{|}"
    show (Position left []) = "{"++(show (map show left))++"|}"
    show (Position [] right) ="{|"++(show (map show right))++"}"
    show (Position left right)
        = "{"++(show(map show left))++"|"++(show (map show right))++"}"


--fun!