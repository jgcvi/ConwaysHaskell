module GoL where
import System.IO

val = [[Alive, Alive], [Alive, Dead]]
val2 = [[Alive, Dead, Alive], [Dead, Alive, Dead], [Alive,Dead,Alive]]

data Cell = Dead | Alive deriving (Show, Eq)
type Board = [[Cell]]

makeMove :: Board -> Board
makeMove = intBoardToBoard . prepareForMove 0

intBoardToBoard :: [[(Int, Cell)]] -> Board
intBoardToBoard = map (\x -> map intToCell x)

intToCell :: (Int, Cell) -> Cell
intToCell (x,c)
	| x == 2 && c == Alive = Alive
	| x == 3 = Alive
	| otherwise = Dead

prepareForMove :: Int -> Board -> [[(Int, Cell)]]
prepareForMove y board = [convertToNeighborCounts 0 y (head $ drop y board) board] ++ rest where
	rest 
		| y + 1 == length board = []
		| otherwise = prepareForMove (y+1) board

getNeighbors :: Int -> Int -> Board -> [Cell]
getNeighbors x y board@(h:t) = xReduced `yReduce` y where
	xReduced
		| x == 0 = [h, head t]
		| x + 1 == length board = [last $ init board, last board]
		| otherwise = take 3 $ drop (x-1) board

yReduce :: Board -> Int -> [Cell]
yReduce board y = concat reduced where
	fn = map (drop (y -1))
	reduced
		| y == 0 = map (take 2) board
		| y + 1== length board = fn board
		| otherwise = map (take 3) $ fn board

convertToNeighborCounts :: Int -> Int -> [Cell] -> Board -> [(Int, Cell)]
convertToNeighborCounts x y row@(h:t) board = (:) ((getNeighborCount x y board, h)) rest where
	rest
		| x + 1 == length board = []
		| otherwise = convertToNeighborCounts (x+1) y t board

getNeighborCount :: Int -> Int -> Board -> Int
getNeighborCount x y board = (length $ filter (== Alive) (getNeighbors x y board)) - val
	where  val 
		| board !! x !! y == Alive = 1
		| otherwise = 0