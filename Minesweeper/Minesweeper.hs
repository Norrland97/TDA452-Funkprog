module Minesweeper where

import Board
import MinesweeperGUI
import Data.Char(isDigit)

type Views = [Pos]

type Flaggs = [Pos]

main =  undefined
{-}
gameLoop :: Board -> Views -> IO()
gameLoop b vs    | win  = showWin
                 | lose = showLose
                 |otherwise = 
             do showView
                s <- getLine
                gameLoop b (vs ++ [toPos s])
    where 
        win  = False
        lose = False
        showWin  = putStr "Win!"
        showLose = putStr "Lose!"
        showView = printView b vs
-}

--prints the coordnates from the board that has been selected
{-}
printView :: Board -> Views -> IO()
printView b vs = printBoard (maskBoard b vs) 

maskBoard :: [Row] -> Views  -> Board
maskBoard rs ps = map updRws rs
    where updRws rw:rws | rw
        {-maskPos p = changeElem ((rs !! snd p) !! fst p) (fst p) (newBoard !! snd p)
          newBoard = emptyBoard (5,5)-}

-- changes an element of a list at a given index
changeElem :: a -> Int -> [a] -> [a]
changeElem v i as = x ++ v : xs 
    where x  = take i as
          xs = drop (i+1) as-}

--read a touple of coordinates from the input --
        --ignores everytnig other than the 2 first Ints--

toPos :: String -> Pos
toPos s = (head ws, ws !! 2)
    where ws = map toInts (words s)
    
toInts :: String -> Int
toInts x
    | all isDigit x = read x
    | otherwise     = 0
---------
{- To calculate time
    starttime <- getClockTime
    call function
    endtime <- getClockTime
    let difftime = diffClockTimes endtime starttime        
-}

