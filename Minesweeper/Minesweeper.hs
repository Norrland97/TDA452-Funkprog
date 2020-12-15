module Minesweeper where

import Board
import MinesweeperGUI
import Data.Char(isDigit)



type Views = [Pos]

type Flaggs = [Pos]

main = gameLoop hiddenExample []

{-gameInit :: IO()
gameInit | bs 
    where bS = do 
            s <- getLine
            let 
-}

-- the game loop
gameLoop :: Board -> Views -> IO()
gameLoop b vs    | lose = showLose 
                 | win  = showWin
                 |otherwise = 
             do printBoard board
                s <- getLine
                gameLoop newBoard (toPos s:vs)
    where 
        newBoard | null vs = b 
                 | otherwise = open b (head vs)
        win  = isWin newBoard
        lose = isLose newBoard
        showWin  = putStr $ "Win!" ++ visibleBoard  -- showView somhow
        showLose = putStr $ "Lose!" ++ visibleBoard -- show bombs sortof
        visibleBoard = "\n" ++ "The Board was: \n" ++  unlines (rowsToString (showAll newBoard))
        

-- returns true if all spaces are showing except bombs
isWin :: Board -> Bool
isWin [] = True
isWin (r:rs) | null rs = rIsWin
             | otherwise = rIsWin && isWin rs
    where rIsWin =not (null [ x | x <- r, item x /= Bomb && state x == Showing ]) 

-- returns true if a bomb is showing
isLose :: Board -> Bool
isLose [] = False
isLose (r:rs) | null rs   = rIsLose
              | otherwise = rIsLose || isLose rs
        where rIsLose = not (null [ x | x <- r, item x == Bomb && state x == Showing])

--read a touple of coordinates from the input --
        --ignores everytnig other than the 2 first Ints--
toPos :: String -> Pos
toPos s = (head ws, ws !! 1)
    where ws =[ x |x <- map toInts (words s), x /= -1]
    
-- converts a string to the integer value or -1 if it contains chars
toInts :: String -> Int
toInts x
    | all isDigit x = read x
    | otherwise     = -1


--------- attempt to calculate the time taken to solve
{- To calculate time
    starttime <- getClockTime
    call function
    endtime <- getClockTime
    let difftime = diffClockTimes endtime starttime        
-}

