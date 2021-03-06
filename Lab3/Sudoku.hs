module Sudoku where

import Test.QuickCheck
import Data.List ((\\),  genericLength, nub,transpose, splitAt, isSuffixOf)
import Data.Maybe (fromJust, mapMaybe, catMaybes, isNothing,isJust)

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

example'' :: Sudoku
example'' =
    Sudoku
      [ [j 9,j 6,j 8,j 1,j 3,j 5,j 2,j 4,j 7]
      , [j 1,j 3,j 7,j 8,j 4,j 2,j 9,j 5,j 6]
      , [j 4,j 2,j 5,j 9,j 6,j 7,j 3,j 8,j 1]
      , [j 7,j 8,j 2,j 6,j 1,j 3,j 4,j 9,j 5]
      , [j 3,j 1,j 4,j 5,j 9,j 8,j 7,j 6,j 2]
      , [j 5,j 9,j 6,j 2,j 7,j 4,j 8,j 1,j 3]
      , [j 8,j 7,j 9,j 3,j 5,j 1,j 6,j 2,j 4]
      , [j 6,j 4,j 1,j 7,j 2,j 9,j 5,j 3,j 8]
      , [j 2,j 5,j 3,j 4,j 8,j 6,j 1,j 7,j 9]
      ]
  where
    n = Nothing
    j = Just

{-}
example' :: Sudoku
example' =
    Sudoku
      [ [j 9,j 6,j 8,j 1,j 3,j 5,j 2,j 4,j 7]
      , [j 1,j 3,j 7,j 8,j 4,j 2,j 9,j 5,j 6]
      , [j 4,j 2,j 5,j 9,j 6,j 7,j 3,j 8,j 1]
      , [j 7,j 8,j 2,j 6,j 1,j 3,j 4,j 9,j 5]
      , [j 3,j 1,j 4,j 5,j 9,j 8,j 7,j 6,j 2]
      , [j 5,j 9,j 6,j 2,j 7,j 4,j 8,j 1,j 3]
      , [j 8,j 7,j 9,j 3,j 5,j 1,j 6,j 2,j 4]
      , [j 6,j 4,j 1,j 7,j 2,j 9,j 5,j 3,j 8]
      , [j 2,j 5,j 3,j 4,j 8,j 6,j 1,j 7,j 9]
      ]
  where
    n = Nothing
    j = Just
-}
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
        where bs           = blocks sudoku 

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
--Returns all the coordinates of the blanks in the sudoku, starting from first row and first column
blanks :: Sudoku -> [Pos]
blanks (Sudoku ss) = [(row, col) |  row <- [0..8], col <- [0..8], isNothing ((ss !! row )!! col)]
         

-- checks if the ammount of positions where there are blanks is as many as the expected number in the 'allBlankSudoku' (works with nub on the list aswell)
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 9*9

-- * E2

-- replaces the index of a list with a given element
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _ = []
l@(x:[]) !!= (i,y) | i == 0    = [y]                           
                   | otherwise = l
l@(x:xs) !!= (i,y) | i < 0                                        
                   || i > (length l-1) = l                            
                   | i == 0            = y : xs 
                   | otherwise         = x : (xs !!= (i-1, y))

-- Checks if !!= works for some values
prop_bangBangEquals_correct :: String -> (Int, Char) -> Bool
prop_bangBangEquals_correct s p@(i, c) | i > length s -1 
                                       || i < 0     = (s !!= p) == s
                                       | otherwise  = ((s !!= p) !! i) == c 

-- * E3

--Updates the soduku at the given position with the given cell
update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (r, c) _ | r < 0 || c < 0
                    || r > 8 || c > 8 = sud 
update (Sudoku rs) pos c = Sudoku (rs !!= (fst pos, newR))
          where newR = (rs !! fst pos) !!= (snd pos,c)

-- tests if the given positions of a sudoku will change with the given cell
prop_update_updated :: Sudoku -> Pos -> Cell -> Property 
prop_update_updated sud (row, col) c = abs row < 9 && abs col < 9 ==> pos (update sud p c) == c --(updateMatrix s (row, col) c !! row) !! col == c
          where p = (abs row, abs col)
                pos s = rows s !! abs row !! abs col

------------------------------------------------------------------------------

type Solution = Maybe Sudoku

-- * F1
-- solves a Sudoku. Returns a Solution, see above
solve :: Sudoku -> Solution
solve s | not (isSudoku s)       = Nothing                                  -- if argument Sudoku is not valid, a.k.a. 9*9, return Nothing
        | isOkay s && isFilled s = Just s                                   -- if the Sudoku is already solved, return a Solution of this s
        | not (isOkay s)         = Nothing                                  -- if the Sudoku is incorrectly filled in there are no Solutions, return Nothing
        | otherwise              = head (solve' s (blanks s))               -- lazy evaluation: solve' is a massive calculation of all solutions, 
                                                                            -- it works because we only ask for the first Solution

-- helper function for the solve function. with recursive calls tihs uptades the positions to possible values and returns a list of all possible solutions if there are any
solve' :: Sudoku -> [Pos] -> [Solution]
solve' sud (p:ps) | isFilled sud && isOkay sud = [Just sud]                             -- success. Sudoku filled in correctly.
                  | null allOptBlank           = []                                     -- no options for p blank space. End recursive path
                  | null ps                    = chopList (map updateBlankIfOk nums)    -- p is now the last blankspace to be filled in
                  | otherwise                  = concatMap sols allOptBlank             -- for every available option for p blank space, drop down and recursively try with next blank space                    
    where 
          sols :: Sudoku -> [Solution]                                                  -- recursively call solve' with sudoku argument n and [Pos] argument ps.
          sols n            = solve' n ps               
          allOptBlank :: [Sudoku]                                                       -- tries every number in next blank space and return valid solutions in list
          allOptBlank       = mapMaybe updateBlankIfOk nums                       
          updateBlankIfOk :: Int -> Solution                                            -- try i in the next blank space. May return Nothing
          updateBlankIfOk i = Just (update sud p (Just i))                          
          nums = numsNotInBlock sud p
        
-- tests if the numbers allready exists in any of the horizontal or vertical blocks
numsNotInBlock :: Sudoku -> Pos -> [Int]
numsNotInBlock sud@(Sudoku rs) (r, c) = ns \\ bs 
        where ns = [1..9] 
              bs = catMaybes (rs !! r) ++ catMaybes (blocksCol sud !! c) ++  catMaybes (blocksBoxS sud (r,c)) 
        
-- Given a sudoku and a position, returns the relative 3by3 block
blocksBoxS :: Sudoku -> Pos -> [Cell]
blocksBoxS (Sudoku rs) (r, c) | c > 2 = blocksBoxS (Sudoku (map (drop 3) rs)) (r, c-3)
                              | r > 2 = blocksBoxS (Sudoku (drop 3 rs)) (r-3, c)
                              | otherwise = concatMap (take 3) (take 3 rs)

-- Removes 'Nothing' elements from list
chopList :: [Maybe a] -> [Maybe a]
chopList ((Just i): xs) = Just i:chopList xs
chopList (Nothing:xs)   = chopList xs
chopList x = []

--updates a given position of a sudoku with a given value if it fits. i.e. if the sudoku is valid after update. if not returns 'Nothing'
tryUpdate :: Sudoku -> Pos -> Int -> Solution   -- try new number in position. If not okay, return Nothing
tryUpdate oldSud pos int | isOkay newSud = Just newSud
                         | otherwise     = Nothing
    where newSud = update oldSud pos (Just int)

-- * F2

--Solves and prints the given sudoku
readAndSolve :: FilePath -> IO ()
readAndSolve path = do 
                      sud <- readSudoku path
                      let solved = solve sud
                      maybe (putStrLn "The givn sudoku is invalid") printSudoku solved

-- * F3
-- checks if Sudoku argument sol solves Sudoku argument sud
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol@(Sudoku sols) sud@(Sudoku suds) = isOkay sol && isOkay sud 
                    && all posEq nonBs && null (blanks sol)
          where nonBs = blanks allBlankSudoku \\ blanks sud 
                posEq p = ((sols !! fst p) !! snd p) == ((suds !! fst p) !! snd p)

-- * F4

--Property for quickcheck, if the solution of a sudoku exists it compares it using isSolutionOf
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isJust sol ==> isSolutionOf (fromJust sol) sud
    where sol = solve sud

--Fewer checks for quickcheck
fewerChecks :: Testable prop => prop -> IO ()
fewerChecks = quickCheckWith stdArgs{maxSuccess=30 }

-- runs the fewerChecks
main = fewerChecks prop_SolveSound