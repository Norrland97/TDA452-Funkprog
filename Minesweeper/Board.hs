module Board where
import System.Random
import Data.List
import Data.Function

data Space = Bomb | Numeric Integer | Blank
    deriving (Eq, Show)

type Row = [Space]

type Board = [Row]

type Pos = (Int,Int)

example =  [[  b,n 1,  l,n 2,  b,n 2],
            [n 1,n 1,  l,n 3,  b,n 2],
            [n 1,n 1,n 2,  b,n 2,n 1],
            [n 1,  b,n 3,n 2,n 2,n 1],
            [n 1,n 2,  b,n 1,n 1, b]]
    where n = Numeric
          b = Bomb
          l = Blank

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

-- construction of a new Board

emptyBoard :: (Int, Int) -> Board
emptyBoard (x,y) = replicate y (replicate x Blank)

{-}
-- printBoard (makeBoard (mkStdGen 88) bombRateMed boardSizeMed)
makeBoard :: StdGen -> Int -> (Int, Int) -> Board
makeBoard g bombAmount size = calcNeighbourScore (snd bombs) (fst bombs)
    where bombs = placeBombs g bombAmount (emptyBoard size)

placeBombs :: StdGen -> Int -> Board -> (Board, [Pos])
placeBombs g bombAmount b = (calcNeighbourScore bomCoord (setBoard b Bomb (sortBy ((on compare snd) <> (on compare fst)) bomCoord)), bomCoord) -- this is where x and y mixup might cause problems
    where bomCoord :: [Pos]
          bomCoord = calcBombCoord g bombAmount (length (head b) ,length b)
-- comparator here is dodgelord no 1 but fuck it i really don't wanna fix it
-}

makeBoard :: StdGen -> Int -> (Int, Int) -> Board
makeBoard g bombAmount size = placeBombs g bombAmount (emptyBoard size)

placeBombs :: StdGen -> Int -> Board -> Board
placeBombs g bombAmount b = calcNeighbourScore boomCord (setBoard b Bomb (sortBy ((on compare snd) <> (on compare fst)) boomCord))  -- this is where x and y mixup might cause problems
    where boomCord = calcBombCoord g bombAmount (length (head b) ,length b)
-- comparator here is dodgelord no 1 but fuck it i really don't wanna fix it


-- test w:  printBoard (setBoard (emptyBoard (boardSizeMed)) Bomb [(0,0), (3,3), (15,15)])
-- could use same strategy as setSpaceInRow. Don't know what's the best
-- sets input space in every coord of input Board defined by input [Pos]
-- demands [Pos] to be sorted
setBoard :: Board -> Space -> [Pos] -> Board
setBoard b@(r:rs) space pos@((x,y):ps) | y == 0    = setBoard ((setSpaceInRow r space x):rs) space ps
                                       | otherwise = r:setBoard rs space (skewPosList ((x,y):ps))       -- unchanged Row r glued to iteration with y-1, positions skewed with 1 
    where
          skewPosList ((xp,yp):pps) = (xp, yp-1):skewPosList pps
          skewPosList []            = []
setBoard b space []                                = b
-- might be index out of bounds above

setSpaceInRow :: Row -> Space -> Int -> Row
setSpaceInRow row sp i | i < 0     = row
                       | otherwise = (take i row) ++ [sp] ++ (drop (i+1) row )


--traverses the board and places numbers on the right space
{-
fillNums :: Board -> Board
fillNums b@(r:rs) = map setNs ns
    where 
        setNs n = uncurry setBoard n
        ns = undefined

-}

calcNeighbourScore :: [Pos] -> Board -> Board
calcNeighbourScore ((x,y):ps) b | ps == []  = allNeighbours (x,y) (-1,-1) b           -- TODO nextNeighbours funkar inte här???
                                | otherwise = calcNeighbourScore ps nextNeighbours
    where nextNeighbours = allNeighbours (x,y) (-1,-1) b

allNeighbours :: Pos -> Pos -> Board -> Board
allNeighbours (x,y) (skewX, skewY) b = recurve (setBoard b successor [((x+skewX), (y+skewY))])
    where successor' (Numeric i) = Numeric (i+1)
          successor' Bomb        = Bomb
          successor' _           = Numeric 1
          successor = successor' ((b !! (y+skewY)) !! (x+skewX))

          nextSkewX :: Int
          nextSkewX | skewX == 1 && skewY == 1  = -2
                    | skewX == 1                = -1
                    | skewX == -1 && skewY == 0 = 1
                    | otherwise                 = skewX + 1
          nextSkewY :: Int
          nextSkewY | skewX == 1                = skewY + 1
                    | otherwise                 = skewY

          recurve nB | nextSkewX == -2 = nB
                     | otherwise       = allNeighbours (x,y) (nextSkewX, nextSkewY) nB


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

          
--placeBombsOnRow ss = 

-- space helpers

-- unnecessary?
typeSpace :: Space -> Space
typeSpace Bomb        = Bomb
typeSpace (Numeric i) = Numeric i
typeSpace Blank       = Blank

-- evaluation a Space in gameplay

isMined :: Space -> Bool
isMined s = s == Bomb

revealSpace :: Board -> (Int, Int) -> Space
revealSpace b (x,y) = typeSpace ((b !! y) !! x)
    
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
showSpace Blank       = "_"
showSpace (Numeric i) = show i
showSpace Bomb        = "*"

