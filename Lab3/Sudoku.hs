module Sudoku where

import Test.QuickCheck
import Data.List ( genericLength, nub,transpose, splitAt )

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example' :: Sudoku
example' =
    Sudoku
      [ [j 3,j 6,j 8,j 8,j 7,j 1,j 2,j 8,j 8]
      , [j 8,j 5,j 8,j 8,j 8,j 8,j 1,j 8,j 8]
      , [j 8,j 8,j 9,j 2,j 8,j 4,j 7,j 8,j 8]
      , [j 8,j 8,j 8,j 8,j 1,j 3,j 8,j 2,j 8]
      , [j 4,j 8,j 8,j 5,j 8,j 2,j 8,j 8,j 9]
      , [j 2,j 7,j 8,j 4,j 6,j 8,j 8,j 8,j 8]
      , [j 8,j 8,j 5,j 3,j 8,j 8,j 9,j 8,j 8]
      , [j 8,j 8,j 3,j 8,j 8,j 8,j 8,j 6,j 8]
      , [j 8,j 8,j 7,j 6,j 9,j 8,j 8,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

exampleRow =
    [j 3,j 6,j 8,j 8,j 7,j 1,j 2,j 8,j 6]
  where
    n = Nothing
    j = Just
-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku allBlankTable
  where allBlankRow    = replicate 9 Nothing
        allBlankTable = replicate 9 allBlankRow

--allBlankSudoku' :: Sudoku
--allBlankSudoku' = Soduku allBlankHelper 9
--  where allBlankRow int = Nothing : allBlankRow
-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = nineLong rs && isRows rs
    where isRows rs | null rs   = True
                    | otherwise = nineLong (head rs) && isRows (tail rs)
          nineLong row = length row == 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku (r:[])) = isRowFilled r
isFilled (Sudoku (r:rs)) = isRowFilled r && isFilled (Sudoku rs)

isRowFilled :: [Maybe a] -> Bool
isRowFilled = all notNull
       where notNull a = not (null a)

--isRowFilled :: Row -> Bool
--isRowFilled (x:[]) = not (null x)
--isRowFilled (x:xs) = not (null x) && isRowFilled xs

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku (r:[])) = do printRow r
printSudoku (Sudoku (r:rs)) = do printRow r 
                                 printSudoku (Sudoku rs)

printRow :: Row -> IO ()
printRow (x:[]) = do putStr (showCell x)
                     putStrLn ""
printRow (x:xs) = do putStr (showCell x)
                     printRow xs

showCell :: Cell -> String
showCell Nothing     = "."
showCell (Just num) = show num

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
        s <- readFile file
        let sud = Sudoku (map readRow (lines s))
        if isSudoku sud
            then return sud
            else error "readSudoku: Not a soduku!"


readRow :: String -> Row
readRow (b:aStr) | b == '.'  = Nothing : aA aStr
                 | otherwise = Just (read [b]) : aA aStr
    where aA [] = []
          aA _  = readRow aStr

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9,no),(1,num)]
    where no  = elements [Nothing]
          num = elements (map Just [1..9])


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do 
        rs <- vectorOf 9 (vectorOf 9 cell)
        return (Sudoku rs)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = length (nub block) == 9


-- * D2

blocks :: Sudoku -> [Block]
blocks sudoku = blocksCol sudoku ++ blocksRow sudoku ++ blocksBox (1,1) sudoku

blocksCol :: Sudoku -> [Block]
blocksCol (Sudoku rs) = transpose rs

blocksRow :: Sudoku -> [Block]
blocksRow (Sudoku rs) = rs

blocksBox :: (Int, Int) -> Sudoku -> [Block]
blocksBox (row, col) (Sudoku rows) | col == -1 = []
                                   | otherwise = concat 
                                                    (third col
                                                        (transpose ( third row rows))) 
                                                            : blocksBox (nR, nC) (Sudoku rows) 
    where nR | col == 3 && row /= 3 = row + 1
             | otherwise = row
          nC | col /= 3 = col + 1
             | row /= 3 = 1
             | otherwise = -1
          third metric input = drop (3*(metric-1))(take (3*metric) input)


test' row col (Sudoku rows) = concat (drop (3*(col-1)) (take (3*col) (transpose ( drop (3*(col-1)) (take (3*row) rows)))))        


prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length bs == 27 && and [length x == 9 | x<-bs]
        where bs          = blocks sudoku 

-- * D3

isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock (blocks sudoku)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
