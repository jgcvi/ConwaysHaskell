val :: Board
val = [[Alive, Alive], [Alive, Dead]]
val2 = [[Alive, Dead, Alive], [Dead, Alive, Dead], [Alive,Dead,Alive]]

--a d a	d a d
--d a d	a d a
--a d a	d a d

data Cell = Dead | Alive
	deriving (Show, Eq)
type Board = [[Cell]]

makeMove :: Board -> Board
makeMove = intBoardToBoard . prepareForMove 0

intBoardToBoard :: [[(Int, Cell)]] -> Board
intBoardToBoard = map (\x -> map intToCell x)

intToCell :: (Int, Cell) -> Cell
intToCell y@(x,c)
	| x == 2 && c == Alive = Alive
	| x == 3 = Alive
	| otherwise = Dead

prepareForMove :: Int -> Board -> [[(Int, Cell)]]
prepareForMove y board = [convertToNeighborCounts 0 y (head $ drop y board) board] ++ rest where
	rest 
		| y + 1 == length board = []
		| otherwise = prepareForMove (y+1) board

getNeighbors :: Int -> Int -> Board -> [Cell]
getNeighbors x y board = xReduced `yReduce` y where
	xReduced
		| x == 0 = [head board, head $ tail board]
		| x == (length board) - 1 = [last $ init board, last board]
		| otherwise = take 3 $ drop (x-1) board

yReduce :: Board -> Int -> [Cell]
yReduce board y = concat reduced where
	reduced
		| y == 0 = map (take 2) board
		| y == (length board - 1) = map (drop (y - 1)) board
		| otherwise = map (take 3) $ map (drop (y - 1)) board

convertToNeighborCounts :: Int -> Int -> [Cell] -> Board -> [(Int, Cell)]
convertToNeighborCounts x y row board = (:) ((getNeighborCount x y board, head row)) rest where
	rest
		| x + 1 == length board = []
		| otherwise = convertToNeighborCounts (x+1) y (tail row) board

getNeighborCount :: Int -> Int -> Board -> Int
getNeighborCount x y board = (length $ filter (== Alive) (getNeighbors x y board)) - val
	where  val 
		| board !! x !! y == Alive = 1
		| otherwise = 0
