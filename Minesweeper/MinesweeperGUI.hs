module MinesweeperGUI where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Canvas
import Data.IORef
import System.Random
import Minesweeper
import Board
import GHC.Float

-- TODO the canvas build is dumb

main :: IO ()
main = startGUI defaultConfig boardSuite

data Minesweeper = MS { size  :: (Int,Int),
                        board :: Board,
                        views :: Views,
                        flagFlag :: Bool,
                        randG :: StdGen}
        deriving (Show)

newMinesweeper :: IO Minesweeper
newMinesweeper = do g <- newStdGen
                    let (i, _) = randomR (0, 5000) g
                    return (MS {size = (8,8), board = hideAll (makeBoard (mkStdGen i) 10 (8,8)), views = [], flagFlag = False, randG = g})

newBoard :: Pos -> Minesweeper -> Minesweeper
newBoard sz state = (MS {size = sz, board = hideAll (makeBoard (randG state) mines sz), views = [], flagFlag = False, randG = (randG state)})
    where mines | sz == (8,8) = bombRateEasy
                | sz == (16,16) = bombRateMed
                | sz == (22,22) = bombRateHard




noGameState :: IO Minesweeper
noGameState = do g <- newStdGen
                 return (MS {size = (0,0), board = hideAll (makeBoard g 0 (0,0)), views = [], flagFlag = False, randG = g})                 

{-}
updateStateBoardCoord :: Minesweeper -> Pos -> IO Minesweeper
updateStateBoardCoord state pos = do return (MS {size = size state,
                                                 board = open (board state) pos,
                                                 views = (views state)})
-}
                                                 
updateStateBoardCoord' :: Pos -> Minesweeper -> Minesweeper
updateStateBoardCoord' pos state = (MS {size = size state,
                                        board = open (board state) pos,
                                        views = (views state),
                                        flagFlag = flagFlag state,
                                        randG = randG state})

updateStateBoardCoordFlag :: Pos -> Minesweeper -> Minesweeper
updateStateBoardCoordFlag pos state = (MS {size = size state,
                                          board = flagOneSpace (board state) pos,
                                          views = (views state),
                                          flagFlag = flagFlag state,
                                          randG = randG state})

updateStateFlagging :: Minesweeper -> Minesweeper
updateStateFlagging state = (MS {size = size state,
                                 board = board state,
                                 views = (views state),
                                 flagFlag = not (flagFlag state),
                                 randG = randG state})                                   

--checkForWin :: Board -> Element -> UI ()       
-- liftIO $ modifyIORef gameStateRef $ updateStateBoardCoord' (doubleToCoord coord)                                           
{-}checkForWin board str | isWin board  = do liftIO $ modifyIORef str strFWin
                      | isLose board = do liftIO $ modifyIORef str strFLose
                      | otherwise = do return ()-}

checkForWin board | isLose board = "oh no :("
                  | isWin board  = "yey you won"
                  | otherwise = "keep going"

strFWin str = "yey you won"

strFLose str = "oh no :( you lost"

tileSize :: Double
tileSize = 30

tileSpaceSize :: Double
tileSpaceSize = 10

{-}
setBoardSize :: Int -> Int -> UI Element
setBoardSize w h = UI.canvas # set UI.width w
                             # set UI.height h
-}

doubleToCoord :: (Double, Double) -> (Int, Int)
doubleToCoord (x,y) = (double2Int (x / (tileSize + tileSpaceSize)), double2Int (y /(tileSize + tileSpaceSize)))

boardSuite :: Window -> UI ()
boardSuite w = 
    do  gameState <- liftIO noGameState                                             -- create initial game State
        return w # set UI.title "~Minesweeper~"                                     -- set title
        getBody w # set style [("background-color", "orange")]                      -- set background color of window

        -- set properties of canvas
        midCanvas <- UI.canvas # set textAlign Center
                               # set textFont "35px sans-serif"
                               # set UI.style[("align-items", "center"),
                                              ("display", "flex")]
                               # set UI.width 1200
                               # set UI.height 640
                              -- # set style [("background-color", "white")]

                -- text styling
        --pure midCanvas # set textFont "35px sans-serif"
        --pure midCanvas # set UI.strokeStyle "gray"
       -- pure midCanvas # set UI.fillStyle   (UI.htmlColor "black")
       -- pure midCanvas # set UI.textAlign Center

        instrText <- string "Welcome to Minesweeper! Please selected desired level of difficulty "
        --startGameButton <- UI.button # set UI.text "Start game"
        --on UI.click startGameButton $ \event ->
        --    do liftIO $ print "sorry, no game implemented"


        -- TODO button styling

        -- buttons for selecting difficulty
        ezGameButton <- UI.button # set UI.text "Ez"
        medGameButton <- UI.button # set UI.text "Medium"
        hardGameButton <- UI.button # set UI.text "X-treme DANGER"


        flagButton <- UI.button # set UI.text "Flag"

        contentGrid <- grid [[ element midCanvas], 
                             [ element instrText],
                             [ element ezGameButton, element medGameButton, element hardGameButton]]
                             # set UI.style[--("padding", "200px"),
                                            ("text-align", "center"),
                                            ("align-items", "center")]

        --midCanvas <- UI.canvas # set UI.width 1800
        --                       # set UI.height 960
        --midCanvas <- setBoardSize 1800 960
        -- create new game State
        gameStateRef <- liftIO $ newIORef gameState
        infoRef <- liftIO $ newIORef instrText


      --  drawBoard midCanvas (0,0) (board gameState)          -- draw game
        getBody w #+ [ element contentGrid]         -- add this list of stuff as children to this window

{-}
        on UI.contextmenu midCanvas $ \coord ->
            do
               element instrText # set UI.text "TJOHO"
               liftIO $ modifyIORef gameStateRef $ updateStateBoardCoord' (doubleToCoord coord) 
               -- liftIO $ modifyIORef hmStateRef (updateHangman c)
               curGameState <- liftIO( readIORef gameStateRef)
               drawBoard midCanvas (0,0) ( (board curGameState))
-}
        -- init canvas click action handler
        on UI.mousedown midCanvas $ \coord ->
            do curGameState <- liftIO( readIORef gameStateRef)
               if (flagFlag curGameState)
               then do liftIO $ modifyIORef gameStateRef $ updateStateBoardCoordFlag (doubleToCoord coord) 
                       curGameState <- liftIO( readIORef gameStateRef)
                       drawBoard midCanvas (0,0) ( (board curGameState))
               --element button # set UI.text "Play again?"
                       element instrText # set UI.text (checkForWin (board curGameState))
               --checkForWin (board curGameState) element
                       return ()
               else do--gameState <- liftIO $ updateStateBoardCoord (gameState) (doubleToCoord coord) 
                       liftIO $ modifyIORef gameStateRef $ updateStateBoardCoord' (doubleToCoord coord) 
               -- liftIO $ modifyIORef hmStateRef (updateHangman c)
                       curGameState <- liftIO( readIORef gameStateRef)
                       drawBoard midCanvas (0,0) ( (board curGameState))
               --element button # set UI.text "Play again?"
                       element instrText # set UI.text (checkForWin (board curGameState))
               --checkForWin (board curGameState) element
                       return ()


        on UI.click flagButton $ \_ ->
            do liftIO $ modifyIORef gameStateRef $ updateStateFlagging
               curGameState <- liftIO( readIORef gameStateRef)
               if (flagFlag curGameState)
               then do element flagButton # set UI.text "Mark blanks"
               else do element flagButton # set UI.text "Flag"
               




        -- init game depending on difficulty level buttons
        on UI.click ezGameButton $ \event ->
            do element midCanvas # set UI.width ((double2Int (tileSize + tileSpaceSize))*8)
               element midCanvas # set UI.height ((double2Int (tileSize + tileSpaceSize))*8)

               --curGameState <- liftIO( readIORef gameStateRef)  

             --  ioReffy <- liftIO $ newIORef ( newBoard (8,8))                                      -- create initial game State
           ---    liftIO ( writeIORef gameStateRef (liftIO (readIORef ioReffy)))
               liftIO $ modifyIORef gameStateRef $ newBoard (8,8)

               contentGrid <- grid [[ element midCanvas], 
                                    [ element instrText],
                                    [ element flagButton]]
                                    # set UI.style[--("padding", "200px"),
                                                   ("text-align", "center"),
                                                   ("align-items", "center")]

               curGameState <- liftIO( readIORef gameStateRef)
               drawBoard midCanvas (0,0) ( (board curGameState))
               getBody w #+ [ element contentGrid] 
            --   element midCanvas # set UI.margin-left 500
        on UI.click medGameButton $ \event ->
            do element midCanvas # set UI.width ((double2Int (tileSize + tileSpaceSize))*16)
               element midCanvas # set UI.height ((double2Int (tileSize + tileSpaceSize))*16)
               gameState <- liftIO newMinesweeper                                          -- create initial game State
               liftIO $ modifyIORef gameStateRef $ newBoard (16,16)

               contentGrid <- grid [[ element midCanvas], 
                                    [ element instrText],
                                    [ element flagButton]]
                                    # set UI.style[--("padding", "200px"),
                                                    ("text-align", "center"),
                                                    ("align-items", "center")]
               curGameState <- liftIO( readIORef gameStateRef)
               drawBoard midCanvas (0,0) ( (board curGameState))
               getBody w #+ [ element contentGrid] 
        on UI.click hardGameButton $ \event ->
            do element midCanvas # set UI.width ((double2Int (tileSize + tileSpaceSize))*22)
               element midCanvas # set UI.height ((double2Int (tileSize + tileSpaceSize))*22)
               liftIO $ modifyIORef gameStateRef $ newBoard (22,22)
                --midCanvas <- setBoardSize 1800 960
                -- create new game State
               --drawBoard midCanvas (0,0) example'          -- draw game
                -- init canvas click action handler
          --     on UI.mousedown midCanvas $ \coord ->
       --            do gameState <- liftIO $ updateStateBoardCoord' (gameState) (doubleToCoord coord)
        --              drawBoard midCanvas (0,0) (board gameState)
                       --return ()
               newContentGrid <- grid [[ element midCanvas], 
                                    [ element instrText],
                                    [ element flagButton]]
                                    # set UI.style[--("padding", "200px"),
                                                    ("text-align", "center"),
                                                    ("align-items", "center")]
               curGameState <- liftIO( readIORef gameStateRef)
               drawBoard midCanvas (0,0) ( (board curGameState))
               element contentGrid # set UI.style[("position", "absolute"),
                                                  ("left", "-3000")]
              -- getBody w # contentGrid
               getBody w #+ [ element newContentGrid] 
                


                -- contentGrid <-                           -- TODO update content grid
                
                
                            -- # set UI.textAlign Center
                            

            -- minesweeperStateRef <- liftIO $ newIORef
        
        return ()





drawBoard :: UI.Canvas -> Point -> Board -> UI ()
drawBoard canvas (x,y) (r:rs) = 
    do drawRow canvas (x,y) r
       drawBoard canvas (x, y+(tileSize + tileSpaceSize)) rs
drawBoard canvas p [] =
    do return ()


drawRow :: UI.Canvas -> Point -> Row -> UI ()
drawRow canvas (x,y) (r:rs) =
    do drawSpace canvas (x,y) r
       drawRow canvas (x+(tileSize + tileSpaceSize), y) rs
drawRow canvas p [] =
    do return ()


drawSpace :: UI.Canvas -> Point -> Space -> UI ()
drawSpace canvas p (Space{state = Flagged}) = 
    do drawBox canvas p "F"
drawSpace canvas p (Space{state = Hidden}) = 
    do drawBox canvas p "-"           
drawSpace canvas p Space{item = Bomb}      =
    do drawBox canvas p "X"
drawSpace canvas p Space{item = Blank}     = 
    do drawBox canvas p ""
drawSpace canvas p Space{item = Numeric i} = 
    do drawBox canvas p (show i)

drawBox :: UI.Canvas -> Point -> String -> UI ()
drawBox canvas p@(x,y) "-" =
    do canvas # set' UI.fillStyle (UI.htmlColor "teal")
       canvas # UI.fillRect p tileSize tileSize
drawBox canvas p@(x,y) str =
    do canvas # set' UI.fillStyle (UI.htmlColor "#a9ddd6")
       canvas # UI.fillRect p tileSize tileSize
       canvas # set' UI.fillStyle (UI.htmlColor "black")
       canvas # UI.strokeText str (x+(tileSize/2), y+(3*tileSize/4))
       canvas # UI.fillText   str (x+(tileSize/2), y+(3*tileSize/4))

{-}
drawBoard :: UI.Element -> Point -> Board -> UI ()
drawBoard canvas (x,y) (r:rs) = 
    do drawRow canvas (x,y) r
       drawBoard canvas (x, y+70) rs
drawBoard canvas p [] =
    do return ()


drawRow :: UI.Element -> Point -> Row -> UI ()
drawRow canvas (x,y) (r:rs) =
    do drawSpace canvas (x,y) r
       drawRow canvas (x+70, y) rs
drawRow canvas p [] =
    do return ()


drawSpace :: UI.Element -> Point -> Space -> UI ()
drawSpace canvas p (Space{state = Hidden}) = 
    do drawBox canvas p ""           
drawSpace canvas p Space{item = Bomb}      =
    do drawBox canvas p "X"

drawSpace canvas p Space{item = Blank}     = 
    do drawBox canvas p "O"
drawSpace canvas p Space{item = Numeric i} = 
    do drawBox canvas p (show i)

drawBox :: UI.Canvas -> Point -> String -> UI ()
drawBox canvas p@(x,y) "" =
    do canvas # set' UI.fillStyle (UI.htmlColor "teal")
       canvas # UI.fillRect p 50 50
drawBox canvas p@(x,y) str =
    do canvas # set' UI.fillStyle (UI.htmlColor "#A9DDD6")
       canvas # UI.fillRect p 50 50
       canvas # set' UI.fillStyle (UI.htmlColor "black")
       canvas # UI.strokeText str (x+25, y+25)
       canvas # UI.fillText   str (x+25, y+25)


     --  pure canvas # set textAlign Center
      -- fillText str (x+25, y+25) canvas
          --    # set UI.textFont    "35px sans-serif"-}
       {-}
       UI.beginPath canvas
       UI.moveTo (0, 50) canvas
       UI.moveTo (50,50) canvas
       UI.moveTo (50, 0) canvas
       UI.moveTo (0,  0) canvas
       UI.closePath canvas
       UI.stroke canvas
       
       canvas # set' UI.lineWidth 10
       canvas # UI.beginPath
       canvas # UI.moveTo p
       canvas # UI.lineTo (30.0, 300.0)
       canvas # UI.closePath
       canvas # UI.fill
       canvas # UI.stroke
       -}
