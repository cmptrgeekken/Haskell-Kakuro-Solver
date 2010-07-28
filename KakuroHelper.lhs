> module KakuroHelper where
> import List(nub,tails,elemIndex,findIndex)

Cell Format = [(<value>,<horiz-sum>,<vert-sum>)]

<value> = [-2..9]
	-2   = empty cell
	-1   = helper cell
	0    = unsolved cell
	1..9 = solved cell

<horiz-sum>|<vert-sum> = 0..45
	0 = non-helper cell
	1..45 = helper cell value

> type KakuroCell = (Integer,Integer,Integer)

Some sample grids follow. These are taken from KrazyDad at:
http://www.krazydad.com/kakuro/

> sample5x51 = [(-2,0,0),(-2,0,0),(-1,0,15),(-1,0,4),(-2,0,0),(-2,0,0),(-2,0,0),(-1,6,0),(0,0,0),(0,0,0),(-1,0,29),(-1,0,15),(-2,0,0),(-1,21,14),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,9,0),(0,0,0),(0,0,0),(-1,14,17),(0,0,0),(0,0,0),(-1,30,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-1,14,0),(0,0,0),(0,0,0),(-2,0,0)]
> sample5x52 = [(-2,0,0),(-2,0,0),(-2,0,0),(-1,0,16),(-1,0,24),(-2,0,0),(-2,0,0),(-1,0,17),(-1,16,19),(0,0,0),(0,0,0),(-2,0,0),(-1,30,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,0,15),(-1,17,0),(0,0,0),(0,0,0),(-1,7,11),(0,0,0),(0,0,0),(-2,0,0),(-1,20,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-1,12,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0)]

> sample8x81 = [(-2,0,0),(-1,0,7),(-1,0,6),(-2,0,0),(-2,0,0),(-1,0,4),(-1,0,13),(-2,0,0),(-2,0,0),(-1,4,0),(0,0,0),(0,0,0),(-1,0,24),(-1,5,8),(0,0,0),(0,0,0),(-1,0,19),(-1,0,10),(-1,44,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-1,14,14),(0,0,0),(0,0,0),(-2,0,0),(-1,3,21),(0,0,0),(0,0,0),(-2,0,0),(-1,3,0),(0,0,0),(0,0,0),(-2,0,0),(-1,3,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-1,16,11),(0,0,0),(0,0,0),(-2,0,0),(-1,17,4),(0,0,0),(0,0,0),(-2,0,0),(-1,13,0),(0,0,0),(0,0,0),(-1,0,17),(-1,6,12),(0,0,0),(0,0,0),(-1,0,11),(-1,0,6),(-1,39,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-1,15,0),(0,0,0),(0,0,0),(-2,0,0),(-1,5,0),(0,0,0),(0,0,0)]
> sample8x82 = [(-2,0,0),(-1,0,9),(-1,0,41),(-2,0,0),(-1,0,7),(-1,0,5),(-2,0,0),(-1,0,39),(-1,0,12),(-1,6,0),(0,0,0),(0,0,0),(-1,3,13),(0,0,0),(0,0,0),(-1,17,10),(0,0,0),(0,0,0),(-1,44,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-1,9,3),(0,0,0),(0,0,0),(-2,0,0),(-1,17,0),(0,0,0),(0,0,0),(-1,0,17),(-1,3,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-1,11,0),(0,0,0),(0,0,0),(-1,7,0),(0,0,0),(0,0,0),(-1,0,12),(-2,0,0),(-2,0,0),(-1,10,16),(0,0,0),(0,0,0),(-2,0,0),(-1,16,3),(0,0,0),(0,0,0),(-1,0,12),(-1,11,14),(0,0,0),(0,0,0),(-1,0,8),(-1,39,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,5,0),(0,0,0),(0,0,0),(-1,17,0),(0,0,0),(0,0,0),(-1,6,0),(0,0,0),(0,0,0)]

> sample10x101 = [(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-1,0,26),(-1,0,9),(-1,0,6),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-1,8,16),(0,0,0),(0,0,0),(0,0,0),(-1,0,17),(-2,0,0),(-1,0,24),(-1,0,14),(-2,0,0),(-1,0,7),(-1,32,18),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,17,0),(0,0,0),(0,0,0),(-1,19,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,12,13),(0,0,0),(0,0,0),(-1,13,10),(0,0,0),(0,0,0),(-1,13,0),(0,0,0),(0,0,0),(-1,16,0),(0,0,0),(0,0,0),(-1,0,7),(-1,17,39),(0,0,0),(0,0,0),(-2,0,0),(-1,3,0),(0,0,0),(0,0,0),(-1,19,5),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,0,17),(-1,0,12),(-2,0,0),(-2,0,0),(-1,17,18),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,3,0),(0,0,0),(0,0,0),(-2,0,0),(-1,10,14),(0,0,0),(0,0,0),(-1,0,6),(-1,6,14),(0,0,0),(0,0,0),(-1,14,16),(0,0,0),(0,0,0),(-1,17,0),(0,0,0),(0,0,0),(-1,4,0),(0,0,0),(0,0,0),(-1,27,11),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-1,6,0),(0,0,0),(0,0,0),(-1,23,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-2,0,0),(-1,23,0),(0,0,0),(0,0,0),(0,0,0),(-2,0,0),(-2,0,0),(-2,0,0)]


Generates a sublist of a list, where the coordinates provided represent the
start and end coordinates of the sublist.

> (!!!) :: [a] -> (Int,Int) -> [a]
> list !!! (from,to) = [v | ind <- [from..to],v <- [list!!ind]]

Predicate that returns true if all elements of the first list are also in
the second list

> allElemOf :: (Eq a) => [a] -> [a] -> Bool
> elems `allElemOf` list = (length $ filter (\x -> x `notElem` list) elems) == 0

Creates all permutations of a certain length from a list

> permute :: (Num a) => a -> [b] -> [[b]]
> permute 0 _ = [[]]
> permute n list = [x:xs | x:xs' <- tails list,xs <- permute (n-1) xs']

Returns the cell value of a Kakuro cell (fst for a three-element list)

> cellValue :: KakuroCell -> Integer
> cellValue (a,_,_) = a

Removes all zero-values from a list

> removeZeros :: (Num a) => [a] -> [a]
> removeZeros = filter (/=0)

Integral square root function (the in-built sqrt function only accepts and
returns floating-point numbers)

> isqrt :: (Integral a) => a -> a
> isqrt = floor . sqrt . fromIntegral

Gets the index of the next empty cell on the board

> getEmptyCellIndex :: [KakuroCell] -> Int
> getEmptyCellIndex board = 
>		case elemIndex (0,0,0) board of
>			Nothing -> (length board - 1)
>			Just a  ->  a

Gets the left and right bounds of the current row or column being solved.

> getIndices :: [KakuroCell] -> Int -> (Int, Int)
> getIndices list elem = (lIndex,rIndex)
>	where
>		fI = findIndex (\(a,_,_) -> a < 0)
>		(left,right) = splitAt elem list
>		lIndex = case fI (reverse left) of
>			Nothing -> 0
>			Just a  -> (length left - 1) - a
>		rIndex = case fI right of
>			Nothing -> length list 
>			Just a  -> (length left) + a

Produces a list of sums for the current section being solved, and returns those
that contain all the elements in the section.

> getSums :: (Num a, Enum a) => [a] -> a -> [a]
> getSums list sum' =  nub $ concat $ filter (\x -> (removeZeros list) `allElemOf` x) (getValues sum' (length list))
>	where
> 		getValues csum ct = filter (\x -> sum x == csum) $ permute ct [1..9]

Gets a column from a 1-Dimensional array.

> get1DColumn :: [a] -> Int -> Int -> [a]
> get1DColumn list col width = [v | ind <- [col,col+width..(length list-1)],v <- [list!!ind]]