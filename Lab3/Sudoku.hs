module Sudoku where

import Test.QuickCheck

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
            else error "Not a sudoku!"


readRow :: String -> Row
readRow (b:aStr) | b == '.'  = Nothing : aA aStr
                 | otherwise = Just (read [b]) : aA aStr
    where aA [] = []
          aA _  = readRow aStr
------------------Tests------------------
--Finally, we need to be able to test properties about the functions related to our Sudokus. In order to do so, QuickCheck needs to be able to generate arbitrary Sudokus.
--Let us split this problem into a number of smaller problems. First, we need to know how to generate arbitrary cell values (of type Maybe Int). Then, we need to know how to generate 81 such cells, and compose them all into a Sudoku.
genSudoku :: Sudoku
genSudoku = Sudoku genRows

genRows :: [Row]
genRows = undefined
--    where   c = oneof [na, rNum]
  --          na = Nothing
    --        rNum = elements $ map Num [1..9]
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = undefined

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = undefined
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock = undefined


-- * D2

blocks :: Sudoku -> [Block]
blocks = undefined

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


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
