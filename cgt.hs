--A game that is Zero (base case) is two empties
--Anything else must be two lists of games, a left list and a right list.

data Game = Position [Game] [Game]

zero = Position [] []

putL :: Game -> Game -> Game
putL goLeft (Position left right) = Position (goLeft:left) right

putR :: Game -> Game -> Game
putR goRight (Position left right) = Position left (goRight:right)

showGame :: Game -> String
showGame (Position [] []) = "{|}"
showGame (Position (l:ls) (r:rs))
    = "{"++(showGame l) ++"|"++(showGame r)++"}"

--fun!