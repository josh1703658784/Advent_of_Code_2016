import Prelude hiding (Left, Right)
import Data.List.Split
--import Data.Char
--import Data.Text hiding (map, splitOn, splitAt)

data Bearing = North | South | East | West deriving (Show, Eq)
data Direction = Left | Right deriving (Show, Eq)


main = do  
    contents <- readFile "input.txt"  
    let formatted = parse $ splitOn ", " contents
    print $ formatted
--    print $ _p2 contents
    

--rotateClockwise = (+90)
--rotateCntrClockwise = (-90)

-- >> INTIALIZATION << --
format :: (String, String) -> (Direction, Int)
format ("L", dist) = (Left, read $ dist :: Int)
format ("R", dist) = (Right, read $ dist :: Int)

parse :: [String] -> [(Direction, Int)]
parse = map (\x -> format $ splitAt 1 x )

-- >> MAIN << --
walkIt :: [(Direction, Int)] -> (Int, Int)
walkIt xs = facing North xs (0,0)

calcBlocksAway :: (Int, Int) -> Int
calcBlocksAway (x,y) = abs $ x+y

facing :: Bearing -> [(Direction, Int)] -> (Int, Int) -> (Int, Int)
facing _ [] pos = pos
--NORTH
facing North ((Left,dist):ds)  (x,y)  = facing West ds (x-dist, y)
facing North ((Right,dist):ds) (x,y)  = facing East ds (x+dist, y)
--SOUTH
facing South ((Left,dist):ds)  (x,y)  = facing East ds (x+dist, y)
facing South ((Right,dist):ds) (x,y)  = facing West ds (x-dist, y)
--EAST
facing East ((Left,dist):ds)  (x,y)   = facing North ds (x, y+dist)
facing East ((Right,dist):ds) (x,y)   = facing South ds (x, y-dist)
--WEST
facing West ((Left,dist):ds)  (x,y)   = facing South ds (x, y-dist)
facing West ((Right,dist):ds) (x,y)   = facing North ds (x, y+dist)
                                                
