--A game that is Zero (base case) is two empties
--Anything else must be two lists of games, a left list and a right list.

--"right" and "left" can replace "getRight" and "getLeft" (eg: right (Position [1] [1] = [1]))
data Game = Position{ right :: [Game], left :: [Game] }

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
    show (Position [] [])      = "{|}"
    show (Position left [])    = "{" ++ showAll left ++ "|}"
    show (Position [] right)   = "{|"++ showAll right++ "}"
    show (Position left right) = "{" ++ showAll left ++ "|" 
                                     ++ showAll right ++ "}"

showAll :: (Show a) => [a] -> String
showAll = concatMap show

instance Num Game where
    negate (Position left right) = Position (map negate right) (map negate left)

    {--
        Addition is messy. Multiplication is worse!
        Basically it's like a cross product. Take two games
        X = {X.L|X.R} and Y = {Y.L | Y.R}

        Then X + Y = {
                      (g + Y for each position g in X.L),
                      (h + X for each position h in y.L)
                      |
                      (i + Y for each position i in X.R),
                      (j + X for each position j in Y.R)
                     }
        Addition is well defined for all games.
        Multiplication is only well defined for numbers--}

    p1@(Position xLeft xRight) + p2@(Position yLeft yRight)
        = Position (map (+p2) xLeft   ++ map (+p1) yLeft)
                   (map (+p2) xRight  ++ map (+p1) yRight)
    --Subtraction is deduced by the compiler! ;)
   {--This allows you to make things like Position [3] [10], a game,
       or half = Position [1,0] [], which is a surreal number.
       Easier than Python!
       Also in Python I concatMauld only make surreal numbers up to 999,
       but with this I was able to do (Position [10000] [])
       (it just took a while to compute)--}
    fromInteger x
        | x > 0 = Position [fromInteger $ pred x] []
        | x < 0 = negate . fromInteger $ negate x
        | otherwise = Position [] []
    signum = undefined
    abs    = undefined
    (*)    = undefined

--instance Eq Game where
--    xGame == yGame = (xGame <= yGame) && (yGame <= xGame)

--instance Ord Game where
--    p1@(Position xL xR) >= p2@(Position yL yR) = all (<= p2) xR && all (p1 <=) yL

-- (foldl (&&) True (map (<= (Position yLeft yRight)) xRight)
-- && (foldl (&&) True (map ((Position xLeft xRight) <=) yLeft)))
