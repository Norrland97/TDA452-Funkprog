module Board where
import System.Random
import Data.List

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

makeBoard :: (Int, Int) -> Board
makeBoard size =undefined --placeBombs (emptyBoard size)

placeBombs :: StdGen -> Board -> Board  
placeBombs g (r:rs) = undefined

-- must give new g each time, mult randomer can't return g
-- calcBombCoord (mkStdGen <any number>) bombRateEasy boardSizeEasy
-- calculates coordinates for bomb placements into tuples in a list
calcBombCoord :: StdGen -> Int -> (Int, Int) -> [Pos]
calcBombCoord gen rate (x,y) = map numToCoord (numsOk rate (x*y) gen)  
    where 
          -- converts a number to a Pos
          numToCoord :: Int -> Pos
          numToCoord n                         = (mod n y, div n y)

          

-- general helpers
-- numsOk bombRateEasy (boardLength*boardHeight) (mkStdGen <any number>) 

numsOk :: Int -> Int -> StdGen -> [Int]
numsOk rate roof g = numsOk' rate roof (nums rate roof g) g

-- gives a <rate> length list with non-duplicated Ints in the span [0, <roof>]
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

