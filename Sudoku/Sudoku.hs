module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List 

-------------------------------------------------------------------------
-- PART A
-------------------------------------------------------------------------
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
			deriving ( Show, Eq )

example0 :: Sudoku
example0 =
    Sudoku
      [ [Just 1, Nothing, Just 1,Just 1,Just 1, Just 1, Just 1, Nothing,Just 1]
      , [Just 2, Just 2, Just 2,Just 2,Just 2, Just 2, Just 2, Just 2,Just 2]
      , [Just 3, Just 3, Just 3,Just 3,Just 3, Just 3, Just 3, Just 3,Just 3]
      , [Just 4, Just 4, Just 4,Just 4,Just 4, Just 4, Just 4, Just 4,Just 4]
      , [Just 5, Just 5, Just 5,Just 5,Just 5, Just 5, Just 5, Just 5,Just 5]
      , [Just 6, Just 6, Just 6,Just 6,Just 6, Just 6, Just 6, Just 6,Just 6]
      , [Just 7, Just 7, Just 7,Just 7,Just 7, Just 7, Just 7, Just 7,Just 7]
      , [Just 8, Just 8, Just 8,Nothing,Just 8, Just 8, Just 8, Just 8,Just 8]
      , [Just 9, Just 9, Just 9,Just 9,Just 9, Just 9, Just 9, Just 9,Just 9]
      ]

example :: Sudoku
example =
	Sudoku
	[ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
	, [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
	, [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
	, [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
	, [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
	, [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
	, [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
	, [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
	, [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
	]
	  
-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku checks if sdk is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sdk = (length (rows sdk) == 9) && (and [(length r == 9 && cellDataCheck r) | r <- rows sdk ])
			where
			cellDataCheck r = and [elem c [1..9] | c <- catMaybes r] 

-- isSolved sdk checks if sdk is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sdk = and [ notElem Nothing r | r <- rows sdk]

-------------------------------------------------------------------------
-- PART B
-------------------------------------------------------------------------
-- printSudoku sdk prints a representation of the sudoku sdk on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sdk = mapM_ putStrLn [toStr line | line <- rows sdk ]
				where 
				toStr line' = concat [ l | l <- map replaceMaybe line']
				replaceMaybe (Just x) = show x
				replaceMaybe Nothing = "."

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do			
					content <- readFile path
					let sdk = Sudoku [[substitueMaybe x | x <- strLine ] | strLine <- (lines content)]						
					if isSudoku sdk 
					then return sdk 
					else error "Program error: Not a Sudoku!"
				where
				substitueMaybe x' | x' == '.' = Nothing
								  | otherwise = Just (digitToInt x')
			
-------------------------------------------------------------------------
-- PART C
-------------------------------------------------------------------------
-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency  [(90, return Nothing), (10, do 
												r <- choose(1,9)
												return (Just r))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- check property whether Sudoku is valid 
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sdk = isSudoku sdk

-------------------------------------------------------------------------
-- PART D
-------------------------------------------------------------------------
type Block = [Maybe Int]

-- this function checks if a block does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock block = length (removeNothing(block)) == length (nub (removeNothing(block)))

-- this function strips 'Nothing' from a block
removeNothing :: Block -> Block
removeNothing []                 			= []
removeNothing (y:ys) 	| y == Nothing    	= removeNothing ys
						| otherwise 		= y : removeNothing ys
	
-- this function returns a list of rows, columns and 9 blocks with size 3x3
blocks :: Sudoku -> [Block]
blocks sdk = rows sdk ++ transpose (rows sdk) ++ segmentBlock sdk

-- this helper function to blocks gives a 3x3 blocks
segmentBlock :: Sudoku -> [Block]
segmentBlock sdk = reCutBlock (take 3 (rows sdk)) 
					++ reCutBlock (take 3 (drop 3 (rows sdk))) 
					++ reCutBlock (drop 6 (rows sdk))
					where
					reCutBlock c = 	[concat (transpose (take 3 $ transpose c))] ++ 
									[concat (transpose(take 3 (drop 3 $ transpose c)))] ++ 
									[concat (transpose (drop 6 $ transpose c))]

-- this property checks if we have the proper number of blocks in a Sudoku
prop_Blocks :: Sudoku -> Bool
prop_Blocks sdk = length (blocks sdk) == 27 && and [length x == 9 | x <- blocks sdk]

-- this function checks whether a block contains duplicate digits
isOkay :: Sudoku -> Bool
isOkay sdk = and [ isOkayBlock x | x <- blocks sdk ]

-------------------------------------------------------------------------
-- PART E
-------------------------------------------------------------------------
type Pos = (Int,Int)

-- this function returns blank positions in a Sudoku
blanks :: Sudoku -> [Pos]
blanks sdk = concat $ bind [ findIndices (== Nothing) r | r <- rows sdk ]
			where
			bind ls = filter (/=[]) [ zip (replicate (length r) (fromJust(elemIndex r ls)) )  r | r <- ls ]

-- this property function checks whether blank contains only the value Nothing
prop_Blanks :: Sudoku -> Bool
prop_Blanks sdk = length( blanks sdk) == countNothing (concat(rows sdk))
				where
				countNothing [] 	= 0
				countNothing (n:ns) = 	if n == Nothing 
										then 1 + countNothing ns 
										else countNothing ns

-- this function updates a list with a new value given the index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) ls (i,v) 	| i < 0 || i >= length ls 	= ls
				| otherwise 				= take i ls ++ [v] ++ drop (i+1) ls

-- this property function checks the length and identicality of the 
-- updated (returned by !!=) and the pre-updated list 
prop_Check :: [Integer] -> (Int,Integer) -> Bool
prop_Check ls (i,v) = and [-- check their length
							length ( ls !!= (i,v) ) == length ls,  
							-- check whether they are identical
							take i ls == take i (ls !!= (i,v)), 
							drop (i+1) ls == drop (i+1) (ls !!= (i,v))] 
				
-- this function updates a Sudoku cell
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sdk (x,y) v 	| not (and [x >= 0, x < 9, y >= 0, y < 9]) = sdk
					| otherwise = Sudoku $ rows sdk !!= (x, (rows sdk !! x) !!= (y, v))

-- this function checks whether the updated position really got the new value,
-- in cases where positions are outside of [0..8], a value 0 will be assigned instead
prop_Update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_Update sdk (x,y) v = 	v == (rows updated !! x') !! y'
							where 
							updated = update sdk (x',y') v
							x' = if x >= 0 && x <= 8 then x else 0
							y' = if y >= 0 && y <= 8 then y else 0

-- this function identifies which numbers can be written in a given position
candidates :: Sudoku -> Pos -> [Int]
candidates sdk (x,y) = [1..9] \\ occupiedValues
						where
						occupiedValues = catMaybes $ (rows sdk !! x) 
										`union` (transpose (rows sdk) !! y ) 
										`union`	(segmentBlock sdk !! identifyBlockNumber (x,y))

-- this function is a helper to candidates in which it returns the index of the block
identifyBlockNumber :: Pos -> Int
identifyBlockNumber (x,y) = head $ (quadrant x matrix) `intersect` (quadrant y (transpose matrix))
						where
						quadrant i m	| i < 0  || i > 8  	= error "the index is out of bound"
										| i >= 0 && i < 3 	= m !! 0 
										| i >= 3 && i < 6 	= m !! 1
										| i >= 6 			= m !! 2
						matrix = [[0,1,2],[3,4,5],[6,7,8]]

-- this function tests whether applying candidates to a Sudoku results a valid suduko
prop_Candidates :: Sudoku -> Pos -> Property 
prop_Candidates sdk (x,y)= isOkay sdk ==> and[ isOkay u && isSudoku u | u <- updated ]
										where
										updated = [ update sdk (x',y') (Just c) | c <- candidates sdk (x',y')] 
										x' = if x >= 0 && x <= 8 then x else 0
										y' = if y >= 0 && y <= 8 then y else 0	
 
-------------------------------------------------------------------------
-- PART F
-------------------------------------------------------------------------
-- this function tries to solve a given Sudoku
solve :: Sudoku -> Maybe Sudoku
solve sud  	| not(isSudoku sud) || not(isOkay sud) = Nothing
			| otherwise = solve' sud
			
-- a helper function to solve - recursively generates blank positions in a Sudoku
solve' :: Sudoku -> Maybe Sudoku
solve' s 	| length (blanks s) == 0 	= Just s
			| otherwise 				= passPosition s (blanks s)

-- this function generate respective candidates for a given position
passPosition :: Sudoku -> [Pos] -> Maybe Sudoku			
passPosition s [] 		= Just s
passPosition s (p:ps)	= passValue s p (candidates s p)

-- this function takes a position and candidate to fill a cell and calls 
-- solve' to fill the next blank cell
passValue :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
passValue s p [] 		= Nothing
passValue s p (v:vs)	= case solve' u of
							Nothing -> passValue s p vs
							Just u  -> Just u
						where 
						u = update s p (Just v)
			
-- this function reads a Sudoku from a file path, solves and returns the result
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
					sdk <- readSudoku path
					let s = solve sdk
					if isJust s 
					then printSudoku $ fromJust s
					else putStrLn "No Solution!"

-- this function checks whether the first Sudoku is the solution of 
-- the second Sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sdk1 sdk2 = isOkay sdk1 
						&& (length $ blanks sdk1) == 0 
						&& (and $ rowCheck (toRows sdk1) (toRows sdk2))
						where
						rowCheck [] [] 				= [True]
						rowCheck (s1:s1s) (s2:s2s) 	= [isNothing s2 || s1 == s2] ++ rowCheck s1s s2s
						toRows s = concat $ rows s

-- this property function checks whether the proposed solution for a 
-- Sudoku is indeed it's solution
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sdk = isOkay sdk ==> fromJust (solve sdk) `isSolutionOf` sdk

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop

