module GoL where

data Cell = Dead | Alive 
	deriving (Show, Eq)
data GameCell = C (Cell, (Int, Int))
instance Show GameCell where 
	show (C (x,_)) = show x 
type Board = [[GameCell]]

makeMove :: Board -> Board
makeMove = neighborBoardToBoard . getNeighborCounts

neighborBoardToBoard :: [[(Int, GameCell)]] -> Board
neighborBoardToBoard = map (\x -> map tupleToCell x)

tupleToCell :: (Int, GameCell) -> GameCell
tupleToCell (count, C (val,c))
	| count == 2 && val == Alive = C (Alive,c)
	| count == 3 = C (Alive,c)
	| otherwise = C (Dead,c)

getNeighborCounts :: Board -> [[(Int, GameCell)]] 
getNeighborCounts b = map (map (\x -> (length (getLiveNeighbors x b), x))) b

getLiveNeighbors :: GameCell -> Board -> [GameCell]
getLiveNeighbors (C (_,(x,y))) b = [vals | vals@(C (Alive, (x1, y1))) <- concat b, 
	   x1 `elem` [(x-1)..(x+1)] && y1 `elem` [(y-1)..(y+1)] && x1 /= x && y1 /= y]
