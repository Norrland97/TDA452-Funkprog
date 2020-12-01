module Sudoku where

import Test.QuickCheck
import Data.List ( genericLength, nub,transpose, splitAt)
import Data.Maybe (catMaybes)

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

exampleRow :: [Maybe Int]
exampleRow =
    [j 3,j 6,j 8,j 8,j 7,j 1,j 2,j 8,j 6]
  where
    n = Nothing
    j = Just

exampleRow' :: [Maybe Int]
exampleRow' =
    [n  ,j 6,j 7,j 3,n  ,j 1,n  ,j 8,j 5]
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
isSudoku (Sudoku rs) = nineLong rs && all nineLong rs && all numRange rs
        where nineLong row = length row == 9
              numRange r = all oneToNine (catMaybes r)
              oneToNine i = i >= 1 && i <= 9
              

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rs) = all isRowFilled rs

-- a helper function which check if a row is completely filled in
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
printSudoku (Sudoku [])     =  return ()
printSudoku (Sudoku rs) = do putStr (unlines (rowsToString rs))

-- Converts a list of rows to a list of strings where the string represents a row        
rowsToString :: [Row] -> [String]
rowsToString = map rowToCell
    where rowToCell r = concatMap showCell r

-- helper function which converts a Cell to a String representation
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
            
-- readSudoku helper reads String to Row
readRow :: String -> Row
readRow (c:aStr) | null aStr = [readCell c]
                 | otherwise = readCell c : readRow aStr

-- converts a char to a Cell
readCell :: Char -> Cell
readCell c | c == '.'   = Nothing
           | otherwise  = Just (read [c])

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9,no),(1,num)]
    where no  = return Nothing
          num = elements (map Just [1..9])


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do 
        rs <- vectorOf 9 (vectorOf 9 cell)
        return (Sudoku rs)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3
-- test function for isSudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
-- checks if a block is correctly filled in
isOkayBlock :: Block -> Bool
isOkayBlock block = length (catMaybes (nub block)) == length (catMaybes block) 


-- * D2
-- converts a Sudoku to 27 Blocks, the task areas of the Sudoku
blocks :: Sudoku -> [Block]
blocks sudoku = blocksCol sudoku ++ blocksRow sudoku ++ blocksBox (1,1) sudoku

-- blocks helper which constructs Blocks representing the Sudoku rows
blocksCol :: Sudoku -> [Block]
blocksCol (Sudoku rs) = transpose rs

-- blocks helper which constructs Blocks representing the Sudoku columns
blocksRow :: Sudoku -> [Block]
blocksRow (Sudoku rs) = rs

-- blocks helper which constructs Blocks representing the Sudoku boxes
blocksBox :: (Int, Int) -> Sudoku -> [Block]
blocksBox (row, col) (Sudoku rows) | col == -1 = []                                                 -- col == -1 is a flag indicating that we are done traversing the sudoku
                                   | otherwise = concat                                             -- concat flattens the 3x3 matrix to a length 9 list
                                                    (third col                                      -- "chops" a third of the sudoku in the other direction
                                                        (transpose ( third row rows)))              -- "chops" a third of the sudoku in the first direction. Then transposes the result from a 3*9 matrix to a 9*3 so that we can "chop higher".
                                                            : blocksBox (nR, nC) (Sudoku rows)      -- recursively continues for the nine boxes of the sudoku
    where nR | col == 3 && row /= 3 = row + 1                                                       -- these calculates the bounds of the next box
             | otherwise            = row
          nC | col /= 3             = col + 1
             | row /= 3             = 1
             | otherwise            = -1
          third metric input        = drop (3*(metric-1))(take (3*metric) input)                    -- this local helper function takes a wanted prefix and then drops an unwanted prefix
                                                                                                    -- e.g. we want the middle part: takes the first six elements, then drops the first three elements of this 

      

-- test for blocks function
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length bs == 27 && and [length x == 9 | x<-bs]
        where bs          = blocks sudoku 

-- * D3
-- checks if the sudoku contains blocks with repeated digits
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
blanks (Sudoku []) = []
--blanks (Sudoku r:rs) = zipLen (blanksHelper r rs)

--blanksHelper :: [a] -> [(a, Int)]
--blanksHelper rs = map zipLen rs
      

zipLen :: [a] -> [(a,Int)]
zipLen as = zip as [1..(length as)]

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =

-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
l@(x:xs) !!= (i,y) | i < 0            = l                            -- error "Can not operate in negative index"
                   | i > (length l-2) = l                            -- error ("Index " ++ (show i) ++ "out of bounds")
                   | i == 0           = y : xs 
                   | otherwise        = x : (xs !!= (i-1, y))

prop_bangBangEquals_correct :: Bool
prop_bangBangEquals_correct = replicate 9 Nothing !!= (2, Just 8)  == [n  ,n  ,j 8,n  ,n  ,n  ,n  ,n  ,n  ]
                           && replicate 9 Nothing !!= (11, Just 8) == [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
                           && replicate 9 Nothing !!= (-4, Just 8) == [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
                           && exampleRow          !!= (2, Just 3)  == [j 3,j 6,j 3,j 8,j 7,j 1,j 2,j 8,j 6]
                           && exampleRow          !!= (28, Just 3) == [j 3,j 6,j 8,j 8,j 7,j 1,j 2,j 8,j 6]
                           && exampleRow          !!= (-3, Just 3) == [j 3,j 6,j 8,j 8,j 7,j 1,j 2,j 8,j 6]
                           && exampleRow          !!= (2, Nothing) == [j 3,j 6,n  ,j 8,j 7,j 1,j 2,j 8,j 6]
    where n = Nothing
          j = Just


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rs) pos c = Sudoku (updateMatrix rs pos c)

updateMatrix :: [Row] -> Pos -> Cell -> [Row]
updateMatrix (r:rs) (row, col) ce | row == 0  = r !!= (col, ce) : rs
                                  | otherwise = r : updateMatrix rs (row-1, col) ce


prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated (Sudoku s) (row, col) c = (updateMatrix s (row, col) c !! row) !! col == c


------------------------------------------------------------------------------

type Solution = Maybe Sudoku

-- * F1
solve :: Sudoku -> Solution
solve s = head (solve' s (blanks s))

solve' :: Sudoku -> [Pos] -> [Solution]
solve' = undefined
-- * F2


-- * F3


-- * F4
