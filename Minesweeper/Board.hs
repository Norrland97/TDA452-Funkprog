module Board where
import System.Random
import Data.List
import Data.Function

-- | Change to a Space being an iten and having a state, hidden showing or flagged
data Item  = Bomb | Numeric Integer | Blank
    deriving (Eq, Show)

data State = Hidden | Showing | Flagged
    deriving (Eq, Show)

data Space = Space {item :: Item, state :: State}
    deriving (Eq, Show)



type Row = [Space]

type Board = [Row]

-- row, col
type Pos = (Int,Int)

example =  [[  b,n 1,  l,n 2,  b,n 2],
            [n 1,n 1,n 1,n 3,  b,n 2],
            [n 1,n 1,n 2,  b,n 2,n 1],
            [n 1,  b,n 3,n 2,n 2,n 1],
            [n 1,n 2,  b,n 1,n 1, b]]
    where n i = Space (Numeric i) Showing
          b = Space Bomb Showing
          l = Space Blank Showing

example' =  [[  h,h,  l,n 2,  b,n 2],
            [h,h,  l,n 3,  b,n 2],
            [h,h,n 2,h,n 2,n 1],
            [n 1,  b,n 3,n 2,n 2,n 1],
            [n 1,n 2,  b,n 1,n 1, b]]
    where n i = Space (Numeric i) Showing
          b = Space Bomb Showing
          l = Space Blank Showing
          h = Space Blank Hidden


winExample =  [[  b,n 1,  l,n 2,  b,n 2],
            [n 1,n 1,  l,n 3,  b,n 2],
            [n 1,n 1,n 2,  b,n 2,n 1],
            [n 1,  b,n 3,n 2,n 2,n 1],
            [n 1,n 2,  b,n 1,n 1, b]]
    where n i = Space (Numeric i) Showing
          b = Space Bomb Hidden
          l = Space Blank Showing


hiddenExample = hideAll example

posExamples :: [Pos]
posExamples = [(1,1),(0,1),(3,4),(0,0),(0,4),(0,3),(4,4)]
exampleRow = [Blank, Blank, Blank, Numeric 1, Bomb, Numeric 1, Blank]

-- constants for amounts of bombs and board sizes for different levels of difficulty

bombRateEasy :: Int
bombRateEasy = 10
boardSizeEasy :: (Int, Int)
boardSizeEasy = (8,8)

bombRateMed :: Int
bombRateMed = 40
boardSizeMed :: (Int, Int)
boardSizeMed = (16, 16)

bombRateHard :: Int
bombRateHard = 99
boardSizeHard :: (Int, Int)
boardSizeHard = (30, 16)

emptyBoard :: (Int, Int) -> Board
emptyBoard (x,y) = replicate y (replicate x (Space Blank Showing))

-----------------------------Logic for creating and setting up boards-------------------
-- creates a board fron given parameters
makeBoard :: StdGen -> Int -> (Int, Int) -> Board
makeBoard g bombAmount size = placeBombs g bombAmount (emptyBoard size)

-- places bombs with a given stdgen and nr
placeBombs :: StdGen -> Int -> Board -> Board
placeBombs g bombAmount b = calcNeighbourScore boomCord (setBoard b Space{item=Bomb, state=Hidden} (sortBy ((on compare snd) <> (on compare fst)) boomCord))  -- this is where x and y mixup might cause problems
    where boomCord = calcBombCoord g bombAmount (length (head b) ,length b)
-- | comparator here is dodgelord no 1 but fuck it i really don't wanna fix it

-- test w:  printBoard (setBoard (emptyBoard (boardSizeMed)) Bomb [(0,0), (3,3), (15,15)])
-- could use same strategy as setSpaceInRow. Don't know what's the best
-- sets input space in every coord of input Board defined by input [Pos]
-- demands [Pos] to be sorted
setBoard :: Board -> Space -> [Pos] -> Board
setBoard b@(r:rs) space ((x,y):ps) | y == 0    = setBoard ((setSpaceInRow r space x):rs) space ps
                                   | otherwise = r:setBoard rs space (skewPosList ((x,y):ps))       -- unchanged Row r glued to iteration with y-1, positions skewed with 1 
    where
          skewPosList ((xp,yp):pps) | yp == 0   = (xp, 0):skewPosList pps   
                                    | otherwise = (xp, yp-1):skewPosList pps
          skewPosList []            = []
setBoard b space []                            = b
setBoard [] _ _                                = []

-- | kommentar??
setSpaceInRow :: Row -> Space -> Int -> Row
setSpaceInRow row sp i | i < 0 
                      || i > ((length row) -1)   = row
                       | i == ((length row) - 1) = (take i row) ++ [sp]
                       | otherwise               = (take i row) ++ [sp] ++ (drop (i+1) row )

-- | Kommentar??
calcNeighbourScore :: [Pos] -> Board -> Board                                         -- shouldn't have weird x y relations
calcNeighbourScore ((x,y):ps) b | ps == []  = allNeighbours (x,y) (-1,-1) b           -- TODO nextNeighbours funkar inte här???
                                | otherwise = calcNeighbourScore ps nextNeighbours    -- recursive call with the rest of the bomb pos. and the updated board
    where nextNeighbours = allNeighbours (x,y) (-1,-1) b

-- bombPos, (-1,-1), board
allNeighbours :: Pos -> Pos -> Board -> Board
allNeighbours (row,sp) (skewRow, skewSp) b = recurve (setBoard b successor [((row + skewRow), (sp + skewSp))])
    where successor' Space{item = Numeric i, state = s} = Space (Numeric (i+1)) s
          successor' Space{item = Bomb, state = s}      = Space Bomb s
          successor' Space{item = _, state = s}         = Space (Numeric 1) s
          successor | okSkewRow && okSkewSp = successor' ((b !! (sp + skewSp)) !! (row + skewRow))

          nextSkewRow :: Int
          nextSkewRow | skewRow == 1 && skewSp == 1  = -2
                      | skewRow == 1                = -1
                      | skewRow == -1 && skewSp == 0 = 1
                      | otherwise                 = skewRow + 1
          nextSkewSp :: Int
          nextSkewSp | skewRow == 1                = skewSp + 1
                     | otherwise                 = skewSp

          okSkewRow | row + skewRow > (length b)                             
                    || row + skewRow < 0               = False
                    | otherwise                        = True
          okSkewSp | sp + skewSp > (length (head b))
                   || sp + skewSp < 0         = False
                   | otherwise                = True
          recurve nB | nextSkewRow == -2 = nB
                     | otherwise         = allNeighbours (row,sp) (nextSkewRow, nextSkewSp) nB

-- must give new g each time, mult randomer can't return g
-- calcBombCoord (mkStdGen <any number>) bombRateEasy boardSizeEasy
-- calculates coordinates for bomb placements into tuples in a list
calcBombCoord :: StdGen -> Int -> (Int, Int) -> [Pos]
calcBombCoord gen rate (x,y) = map numToCoord (numsOk rate (x*y-1) gen)  
    where 
          -- converts a number to a Pos
          numToCoord :: Int -> Pos
          numToCoord n                         = (mod n y, div n y)
          
-- general helpers -----------------------------------------------------------------------
-- numsOk bombRateEasy (boardLength*boardHeight) (mkStdGen <any number>) 
-- gives a <rate> length list with non-duplicated Ints in the span [0, <roof>]
numsOk :: Int -> Int -> StdGen -> [Int]
numsOk rate roof g = numsOk' rate roof (nums rate roof g) g

numsOk' :: Int -> Int -> [Int] -> StdGen -> [Int]
numsOk' rate roof oldList g3 | length nL == rate = nL
                             | otherwise         = numsOk' rate roof (numsNoDup nL g3) (snd (updStdGen g3))
    where -- oldList without duplicates 
          nL                 = nub oldList
          -- fill out full length with nums, input missing length and an updated StdGen
          numsNoDup :: [Int] -> StdGen -> [Int]
          numsNoDup nNDFl g2 = nNDFl ++ nums (rate - length nNDFl) roof (snd (updStdGen g2))
          -- since randomRs don't update the StdGen in ghci belows only purpose is to update an StdGen
          updStdGen :: StdGen -> (Int, StdGen)
          updStdGen g4       = randomR (0,10) g4

-- get <amount> random numbers in the span [0,<roof>]
nums :: Int -> Int -> StdGen -> [Int]
nums amount roof g = take amount (randomRs (0, roof) g)  
----------------
----------------------------------Logic for changing states of the spaces of a board-------------
-- shows all the spaces
showAll :: Board -> Board
showAll [] = []
showAll (r:rs) = map show r:showAll rs
    where show Space{item = i, state = s} = Space i Showing

-- hides all the spaces
hideAll :: Board -> Board
hideAll [] = []
hideAll (r:rs) = map hid r:hideAll rs
    where hid Space{item = i, state = s} = Space i Hidden          

--gets the items posioned above, under and to each side of the position, including position
getAdjacent :: Board -> Pos -> [Pos]
getAdjacent rs (col, row) = [(c,r)|r <- [row-1..row+1], c <- [col-1..col+1], inLimit r && inLimit c]
    where inLimit i = i >= 0 && i <= 8

-- reveals a space, if the space is a blank, it reveals all surounding blsnks and surrounding numbers
open :: Board -> Pos -> Board
open b p@(col, row) = revealSeveralSpace b op 
    where space = (b !! row) !! col
          op    = open' b p space

-- returns a list of positions to open  
open':: Board -> Pos -> Space -> [Pos] 
open' _ p Space{state = Flagged} = [(-1,-1)]
open' b p Space{item = Blank}    = nub $ concat [(getAdjacent b x)| x <- getAdjacent b p, item ((b !! snd x) !! fst x) == Blank]
open' _ p _                      = [p] -- otherwise is num or Bomb, in that case open one

-- evaluation a Space in gameplay, set a space to show
revealSpace :: Board -> (Int, Int) -> Space
revealSpace b (col,row) = Space (item ((b !! row) !! col)) Showing

-- reveals a space of a given borad
revealOneSpace :: Board -> Pos -> Board
revealOneSpace b@(r:rs) (x,y) | x < 0 || y < 0 = b -- guards to return unchanged Board if index too large or small
                              | x > length b || y > length r = b
revealOneSpace b (col, row) = b !!= (row, newRow)
    where newRow = (b !! row) !!= (col, revealSpace b (col,row))

--Reveals multiple spaces and returns the result as a board
revealSeveralSpace :: Board -> [Pos] -> Board
revealSeveralSpace b (p:[]) = revealOneSpace b p
revealSeveralSpace b (p:ps) = revealSeveralSpace new ps
    where new = revealOneSpace b p

--Flags a given space if it's not allread flaggen, otherwise removes flag
flagSpace :: Board -> Pos -> Board
flagSpace = undefined
---------------------------------------------------------
-----------a 'nice to have' operator
-- replaces the index of a list with a given element
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _ = []
l@(x:[]) !!= (i,y) | i == 0    = [y]                           
                   | otherwise = l
l@(x:xs) !!= (i,y) | i < 0                                        
                   || i > (length l-1) = l                            
                   | i == 0            = y : xs 
                   | otherwise         = x : (xs !!= (i-1, y))
--------------------
------------------------------------Logic to print boards
--prints the board
printBoard :: Board -> IO()
printBoard [] = return ()
printBoard rs = do putStr (unlines (rowsToString rs))

-- Converts a list of rows to a list of strings where the string represents a row        
rowsToString :: [Row] -> [String]
rowsToString = map rowToSpace
    where rowToSpace r = concatMap showSpace r

-- helper function which converts a Cell to a String representation
showSpace :: Space -> String
showSpace Space{state = Hidden}   = "O"
showSpace Space{state = Flagged}  = "F"
showSpace Space{item = Blank}     = "_"
showSpace Space{item = Numeric i} = show i
showSpace Space{item = Bomb}      = "*"
-----------------------------------
