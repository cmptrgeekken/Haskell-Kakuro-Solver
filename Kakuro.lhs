> module Kakuro where
> import Data.List(intersect)
> import KakuroHelper

The Kakuro solver is divided into two sections. This section provides the
backtracking algorithm used for solving a Kakuro puzzle. There are four 
functions:

choices - Produces a list of valid moves for the next element in the Kakuro grid
choose  - Selects the provided move and returns a new board with the selection
          made
solved  - Returns true if there are no more moves to be made on the board
solve   - Runs the solving algorithm on a board, and returns the solution
          or an empty list if no solutions exist.
		  
The other section is KakuroHelper. This contains all the helper functions
used for the Kakuro solver. It also contains several sample Sudoku grids
that can be used to test the algorithm. To run the solver over a grid,
pass the grid into the solve function:

solve sample5x51

--------------------------------------------------------------------------------

The Choices algorithm works similarly to a Sudoku solver, where a value
is 'safe' as long as it meets three requirements. Of course, the requirements
are slightly different:

a) It has not been used at all in the current row context.
b) It has not been used at all in the current column context.
c) It must be present in a list of values representing the sums of the current
   row context and column context.
   
There are some challenges that make solving Kakuro slightly more difficult
than solving Sudoku. These include:

1) The rows and columns are not of uniform length, like in Sudoku.
2) It is possible for a single row or column to represent multiple sections
   that need to be solved individually. Thus, we have to extract each section
   based on its context.
3) Boards can be of any dimension, as long as they're square. Thus, its not 
   possible to hard-code dimensions.

> choices :: [KakuroCell] -> [Integer]
> choices board = [r | r <- [0..9],safe r]
>	where
>		safe r = r `notElem` rowVals && r `notElem` colVals && r `elem` sums
>			where
>			-- Calculating row and column number is done the same way it is
>			-- done in Sudoku.
>			rowNum a = (a `div` boardWidth)
>			colNum a = (a `mod` boardWidth)
>
>			-- The current cell number is given by the next unsolved cell
>			cell = getEmptyCellIndex board
>
>			-- The board with is square root of the length of the board array.
>			-- This requires the board to be square in dimensions. Otherwise,
>			-- the user would have to also provide a width argument when
>			-- passing in a board.
>			boardWidth = isqrt $ length board
>
>			-- The current column. This will be pruned later on
>			-- so that it only contains the context for the 
>			-- current section being solved.
>			column = get1DColumn board (colNum cell) boardWidth
>
>			-- Gets the left and right bounds of the current section
>			-- for both the row and the column. The left bounds lie
>			-- on the helper cell that describe the required sum for
>			-- the current row. The right bound is one index beyond
>			-- the end of the section.
>			(rLIndex,rRIndex) = getIndices board cell
>			(cLIndex,cRIndex) = getIndices column (rowNum cell)
>
>			-- Gets the current required sums for the section. It is assumed 
>			-- that both a row and column sum have been provided for every
>			-- cell on the board. If this is not the case, an error
>			-- will occur at this point.
>			(-1,rSum,_) = board!!rLIndex
>			(-1,_,cSum) = column!!cLIndex
>
>			-- Generates lists containing all current values for the cells
>			-- in the section.
>			rowVals = map cellValue board!!!(rLIndex+1,rRIndex-1)
>			colVals = map cellValue column!!!(cLIndex+1,cRIndex-1)
>
>			-- Gets a list of allowed values (based on the sums) for the
>			-- row and column
>			rValues = getSums rowVals rSum
>			cValues = getSums colVals cSum
>
>			-- The list of sums is merely the intersection of the horizontal
>			-- and vertical sums.
>			sums = rValues `intersect` cValues 

Choosing a value simply places the value into the next empty slot on the board.
This requires splitting the board at the point of insertion and gluing it back
together with the new value in place.

> choose :: [KakuroCell] -> Integer -> [KakuroCell]
> choose board val = (leftBoard) ++ [(val,0,0)] ++ (rightBoard)
>	where
>		cell = getEmptyCellIndex board
>		splitBoard = splitAt cell board
>		leftBoard  = fst splitBoard
>		rightBoard = case snd splitBoard of
>						[] -> []
>						a  -> tail a

A puzzle is solved if it contains no zero values.

> solved :: [KakuroCell] -> Bool
> solved = (notElem (0,0,0))

Performs a simple backtracking algorithm until a solution is found. Returns the
first solution discovered.

> solve :: [KakuroCell] -> [KakuroCell]
> solve puzzle = 
>	case solved puzzle of
>		True -> puzzle
>		otherwise -> 
>			case filter (/=[]) [solve $ choose puzzle r | r<-choices puzzle] of
>				[] -> []
>				puzzle -> (head puzzle)