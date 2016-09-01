module Kakuro where
import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Array
import Test.QuickCheck
import Data.Char (isSpace,digitToInt)

-----------------------------------------------------------------------------
-- MODELLING KAKURO AND IT'S OBJECTS
-----------------------------------------------------------------------------
-- Modelling elements of Kakuro
type Square = (Maybe Int,Maybe Int)

-- Modelling Kakuro
data Kakuro = Kakuro { rows :: [[Square]] } 
			deriving (Show,Eq)

-- Modelling types of Squares			
data SquareType = Corner | Side | Clue | Cell | Fcell
				deriving (Show,Eq)
				
-- Modelling a block
type Block = [(Maybe Int,Maybe Int)]

-- Modelling positions
type Pos = (Int,Int)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- UTILITY FUNCTIONS FOR CANDIDATE
-----------------------------------------------------------------------------
-- this function decides what a square is 
squareType :: Square -> SquareType
squareType (Nothing, Nothing) 	= Corner
squareType (Just _, Nothing)	= Side
squareType (Nothing, Just _)	= Side
squareType (Just 0, Just 0)		= Cell
squareType (Just _, Just 0)  	= Fcell
squareType (_,_) 				= Clue

-- this property checks squareType
prop_squareType :: Square -> Bool
prop_squareType sqr = squareType sqr `elem` [Corner,Side,Clue,Cell,Fcell]
					
-- this function given a square returns its value
squareValue :: Square -> [Int]
squareValue sqr = case squareType sqr of
					Side 	-> if v2 == Nothing then [fromJust v1,0] else [0,fromJust v2]
					Clue 	-> [fromJust v1, fromJust v2]
					Cell 	-> [fromJust v1]
					Fcell 	-> [fromJust v1]
				where
					(v1,v2) = sqr

-- this function takes a position in Kakuro and returns its corresponding square
getSquare :: Kakuro -> Pos -> Square
getSquare kakr (r,c) = head (drop c (head $ drop r (rows kakr)))

-- this function takes a block and returns the indexes of the cells and fcells in the block
indexOfFCell :: Block -> [Int]
indexOfFCell b = findIndices (== Fcell) (blockType b) `union` findIndices (== Cell) (blockType b)
				where
				blockType row = [ squareType r  | r <- row]

-- this statement selects the group that contains the column
select :: Int -> [[Int]] -> [Int]
select r [] 		= []
select r (i:is)		| elem r i 	= i
					| otherwise = select r is
						
-- this function given a Kakuro and a position for a square; it returns vertical (|) block of CELL and FCELL
downBlock :: Kakuro -> Pos -> Block
downBlock kakr (r,c) = [getSquare kakr i | i <- zip block (replicate (length block) c ) ]
						where
						block = select r (groupNumbers (sort $ indexOfFCell column))
						-- this statement returns the specific column
						column = head $ drop c (transpose (rows kakr))
						
-- this function given a list of numbers, it group consecutive numbers together
groupNumbers :: (Enum a, Eq a) => [a] -> [[a]]
groupNumbers = foldr val []
				where
				val x (ys@(y:_) : zs) | succ x == y = (x:ys) : zs
				val x t = [x] : t

-- this test property checks whether groupNumbers returns a consecutive numbers
prop_groupNumbers :: [Int] -> Property
prop_groupNumbers n = not (null n) ==> and [ length e == len e | e <- groupNumbers n ]
					where
					len e = (maximum e) - (minimum e) + 1	
					
-- this function given a Kakuro and a position for a square; it returns horizontal (-) blocks of CELL and FCELL
acrossBlock :: Kakuro -> Pos -> Block
acrossBlock kakr (r,c) = [getSquare kakr i | i <- zip (replicate (length block) r ) block]
							where
							block = select c ( groupNumbers (sort $ indexOfFCell row))
							-- this statement returns the specific row
							row = (head $ drop r (rows kakr))
							
-- this function returns blank positions in a Kakuro
blankCells :: Kakuro -> [Pos]
blankCells kakr = concat $ bind [ findIndices (== Cell) (blockType r) | r <- rows kakr ]
				where
				bind ls = [ zip (replicate (length $ snd e) (fst e)) (snd e) | e <- zip [0..] ls ]
				blockType row = [ squareType r  | r <- row]
				
-- this function returns an across clue number for a given Kakuro and CELL/FCELL position
acrossClue :: Kakuro -> Pos -> Int
acrossClue kakr (r,c) = fromJust $ choose (getSquare kakr cp)
						where
						choose (a,b) = b
						-- this statement gives the first left encountered non cell square
						cp = if length block == 0 then error "not valid" else (r,(head block) - 1)
						-- this statement gives the index of cells
						block = select c (groupNumbers (sort $ indexOfFCell row))
						-- this statement gives the specific row
						row = (head $ drop r (rows kakr))
						
-- this function returns the top clue number for a given Kakuro and CELL/FCELL position
downClue :: Kakuro -> Pos -> Int
downClue kakr (r,c) = fromJust $ choose (getSquare kakr cp)
					where
					choose (a,b) = a
					-- this statement gives the first top encountered non cell square
					cp = if length block == 0 then error "not valid" else ((head block) - 1,c)					
					-- this statement gives the index of cells
					block = select r (groupNumbers (sort $ indexOfFCell column))
					-- this statement returns the specific column
					column = head $ drop c (transpose (rows kakr))
					
-- this function given a number and number of runs; it returns the list of possible numbers
-- between 1 and 9 that sum up to the number
possibleNumbers :: Int -> Int -> [[Int]]
possibleNumbers n r = [ p | p <- partitions n r, length p == length (nub p)]

-- this test property function checks possibleNumbers
prop_possibleNumbers :: Int -> Int -> Bool
prop_possibleNumbers n r = and [ foldr (+) 0 e == n' | e <- possibleNumbers n' r' ]
						where
						n' = if n > 2 && n < 46 then n else 3
						r' = if r > 1 && r < 10 then r else 2

-- this is a helper function to possibleNumbers in which it generates the partition of
-- a positive integer. 
partitions :: Int -> Int -> [[Int]]
partitions m n = table ! (m, n, 9)
				where
				table   = listArray ((1, 1, 1), (m, n, 9)) l
				l       = [f i j k | i <- [1 .. m], j <- [1 .. n], k <- [1 .. 9]]
				f i 1 k = if i > k `min` 9 then [] else [[i]]
				f i j k = [d : ds | d <- [1 .. k `min` pred i], ds <- table ! (i - d, j - 1, d)]
				
-- this function update a cell given a Kakuro, value and position
updateKakuro :: Kakuro -> Pos -> Square -> Kakuro
updateKakuro kakr (r,c) s = Kakuro $ rows kakr !!= (r, (rows kakr !! r) !!= (c, s))

-- this function updates a list with a new value given the index
-- (we used this function in our Sudoku assignment for same purpose)
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) ls (i,v) 	| i < 0 || i >= length ls 	= ls
				| otherwise 				= take i ls ++ [v] ++ drop (i+1) ls
				
-- this property function checks whether the list length is unchanged in !!= operation
prop_Insert :: [Int] -> (Int,Int) -> Bool
prop_Insert ls (i,v) = length ( ls !!= (i,v) ) == length ls 
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- THE CANDIDATE FUNCTION
-----------------------------------------------------------------------------
-- this function gives back possible candidates for a given CELL
candidates :: Kakuro -> Pos -> [Int]
candidates kakr p = ps \\ pu
					where
					-- this statement gives list of numbers that can be used in a cell considering duplication
					pu = nub $ au `union` du
					au = [ head $ squareValue d | d <- acrossBlock kakr p]
					du = [ head $ squareValue d | d <- downBlock kakr p]
					-- this statement gives list of numbers that can be used in a cell considering sum
					ps = nub $ as `intersect` ds
					as = concat $ possibleNumbers ( (acrossClue kakr p) - sum au ) ((length $ acrossBlock kakr p) - length [ z | z <- au, z /= 0])
					ds = concat $ possibleNumbers ( (downClue kakr p) -  sum du ) ((length $ downBlock kakr p) -  length [ z | z <- du, z /= 0])
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- BACKTRACKING ALGORITHM FOR SOLVING KAKURO
-----------------------------------------------------------------------------
-- this function checks whether a given Kakuro got equal width in its rows
isValid :: Kakuro -> Bool
isValid kakr = and [length ( head $ rows kakr) == length r | r <- rows kakr]

-- this function check if a Kakuro is valid before trying to solve
solve :: Kakuro -> Maybe Kakuro
solve kakr = if isValid kakr then solveIt kakr else Nothing
		
-- a function to solve - recursively generates blank positions
solveIt :: Kakuro -> Maybe Kakuro
solveIt kakr 	| length (blankCells kakr) == 0 	= Just kakr
				| otherwise 						= passPosition kakr (blankCells kakr)

-- this function generate respective candidates for a given position
passPosition :: Kakuro -> [Pos] -> Maybe Kakuro			
passPosition kakr [] 		= Just kakr
passPosition kakr (p:ps)	= passValue kakr p (candidates kakr p)
							
-- this function takes a position and candidate to update a cell
passValue :: Kakuro -> Pos -> [Int] -> Maybe Kakuro
passValue kakr p [] 		= Nothing
passValue kakr p (v:vs)		= case solveIt u of
								Nothing -> passValue kakr p vs
								Just u  -> Just u
							where
								u = updateKakuro kakr p (Just v,Just 0)
-----------------------------------------------------------------------------
				
-----------------------------------------------------------------------------
-- THE IO STUFF
-----------------------------------------------------------------------------
-- this function given a Kakuro it prints the board
displayBoard :: Kakuro -> IO ()
displayBoard kakr = mapM_ putStrLn [concat $ board line | line <- rows kakr ]

-- this function saves a Kakuro board to a file
saveBoard :: Kakuro -> String -> IO ()
saveBoard kakr path = writeFile path (str [concat $ board line | line <- rows kakr ])
							where 
							str [] 		= ""
							str (l:ls) 	= l ++ "\n" ++ str ls
							
-- this function read an empty Kakuro from a file
readKakuroBoard :: String -> IO Kakuro
readKakuroBoard path =  do			
							board <- readFile path
							let k = Kakuro [ [bind e |e <- (tak line)] | line <- (lines board)]
							if isValid k 
							then return k 
							else error "The file does not contain a valid Kakuro board!"
						where
						tak [] = []
						tak l = take 6 l : tak (drop 6 l)
							
-- this helper function to readKakuroBoard accepts a String and turns them into Square
bind :: String -> Square						
bind s 	| t == "#" 						= (Nothing,Nothing)
		| t == "" 						= (Just 0,Just 0)
		| elemIndex '\\' t /= Nothing 	= (if a == "" then Just 0 else Just (read a :: Int), 
											if drop 1 b == "" then Just 0 else Just (read (drop 1 b) :: Int))
		| otherwise 					= error "The file has some errors!" 
		where
			t = trim (drop 1 s)
			(a,b)= splitAt (fromJust(elemIndex '\\' t)) t
			
-- this is a helper function to readKakuroBoard in which it trim strings
-- the functions is adopted from wikipedia
trim :: String -> String
trim = f . f
		where f = reverse . dropWhile isSpace
		
-- this function takes a board, solve it and write it to file
readSolveSave :: String -> String -> IO ()
readSolveSave inPath outPath= do
							kakr <- readKakuroBoard inPath
							let k = solve kakr
							if isJust k
							then saveBoard (fromJust k) outPath
							else error "Nothing"

-- this function formats the elements in Kakuro board for better visualization
board :: Block -> [String]
board row = ["|" ++ format (check s') | s' <- row]
			where
			check s = case squareType s of 
						Corner 	-> "#" 
						Clue 	-> show ((squareValue s)!!0) ++ "\\" ++ show ((squareValue s)!!1)
						Cell 	-> " "
						Fcell 	-> show ((squareValue s)!!0)
						Side 	->  if (squareValue s)!!0 == 0
								then "\\" ++ show ((squareValue s)!!1)
								else  show ((squareValue s)!!0) ++ "\\"
				
-- this is a helper function for board which gives back well formatted string 
format :: String -> String
format x = concat [ f | f <- replicate n " "] ++ x 
			++ concat [ b | b <- replicate (3 - len) " ", 3 > len ]
			where
			len = length x
			n = if 3 > len then 2 else (5 `mod` len)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- SAMPLE KAKURO PUZZLES
-----------------------------------------------------------------------------
-- easy kakuro
example :: Kakuro
example = 
	Kakuro
	  [ [(Nothing,Nothing), (Just 15,Nothing), (Just 13,Nothing), (Nothing, Nothing),(Just 17,Nothing), (Just 4,Nothing), (Just 11,Nothing), (Nothing,Nothing)]
	  , [(Nothing,Just 3), (Just 0,Just 0), (Just 0,Just 0), (Just 5, Just 11),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 23,Nothing)]
	  , [(Nothing,Just 42), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)] 
	  , [(Nothing,Just 7), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 23, Nothing),(Just 6, Just 12), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing, Just 4), (Just 0,Just 0),(Just 0,Just 0),(Just 6, Just 23), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing, Nothing), (Just 9, Nothing), (Just 30, Just 9),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 29, Nothing), (Just 11, Nothing)]
	  , [(Nothing, Just 26), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 15, Just 8), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing, Just 7), (Just 0,Just 0), (Just 0,Just 0), (Just 13, Nothing), (Just 7, Just 20), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing,Just 28), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing,Nothing), (Nothing, Just 23), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0),(Nothing,Just 14),(Just 0,Just 0),(Just 0,Just 0)]
	  ]

-- hard Kakuro
example1 :: Kakuro
example1 = 
	Kakuro
	  [ [(Nothing,Nothing), (Just 23,Nothing), (Just 6,Nothing), (Nothing, Nothing),(Nothing,Nothing), (Nothing,Nothing), (Just 7,Nothing), (Just 23,Nothing)]
	  , [(Nothing,Just 11), (Just 0,Just 0), (Just 0,Just 0), (Just 41, Nothing),(Nothing,Nothing), (Just 29,Just 11), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing,Just 9), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 9,Just 15),(Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0)] 
	  , [(Nothing,Just 42), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing, Nothing), (Nothing, Nothing),(Just 16, Just 7),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 17,Nothing), (Nothing,Nothing)]
	  , [(Nothing, Nothing), (Just 23, Just 6), (Just 0, Just 0),(Just 0,Just 0), (Just 10,Just 3), (Just 0,Just 0), (Just 0,Just 0), (Just 24, Nothing)]
	  , [(Nothing, Just 41), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing, Just 29), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing,Just 9), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Nothing,Just 22),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	  , [(Nothing,Just 12), (Just 0,Just 0), (Just 0,Just 0), (Nothing,Nothing), (Nothing,Nothing),(Nothing,Just 11),(Just 0,Just 0),(Just 0,Just 0)]
	  ]

-- hard Kakuro
example2 :: Kakuro
example2 = Kakuro
	[[(Nothing,Nothing), (Nothing,Nothing), (Just 38,Nothing), (Just 3,Nothing), (Just 13,Nothing), (Just 10,Nothing), (Just 21,Nothing), (Nothing,Nothing)]
	,[(Nothing,Nothing), (Just 11,Just 32), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 5, Nothing)]
	,[(Nothing,Just 29), (Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	,[(Nothing,Just 13), (Just 0,Just 0), (Just 0,Just 0), (Just 32,Nothing), (Just 10,Nothing), (Just 15,Just 3), (Just 0,Just 0), (Just 0,Just 0)]
	,[(Nothing,Nothing), (Just 8,Just 16), (Just 0,Just 0),(Just 0,Just 0),(Just 0,Just 0),(Just 0,Just 0),(Just 0,Just 0),(Just 16, Nothing)]
	,[(Nothing,Just 41), (Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	,[(Nothing,Just 28), (Just 0,Just 0), (Just 0,Just 0),(Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	,[(Nothing,Nothing), (Just 11,Nothing), (Just 6,Just 13), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Just 3,Nothing), (Just 13,Nothing)]
	,[(Nothing,Just 7), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0), (Nothing,Just 8), (Just 0,Just 0), (Just 0,Just 0), (Just 0,Just 0)]
	,[(Nothing,Just 14), (Just 0,Just 0), (Just 0,Just 0), (Nothing,Nothing),(Nothing,Nothing), (Nothing,Just 11), (Just 0,Just 0), (Just 0,Just 0)]
	]
-----------------------------------------------------------------------------
