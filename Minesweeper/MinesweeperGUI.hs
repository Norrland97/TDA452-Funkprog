module MinesweeperGUI where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Canvas
import Data.IORef
import System.Random
import Minesweeper
import Board
import GHC.Float


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


checkForWin board | isLose board = "Oh no, you lost!"
                  | isWin board  = "Yey, you won!"
                  | otherwise = "Keep going!"

strFWin str = "yey you won"

strFLose str = "oh no :( you lost"

tileSize :: Double
tileSize = 30

tileSpaceSize :: Double
tileSpaceSize = 10



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


        instrText <- string "Welcome to Minesweeper! Please selected desired level of difficulty "

        ezGameButton <- UI.button # set UI.text "Ez game"
        medGameButton <- UI.button # set UI.text "Medium"
        hardGameButton <- UI.button # set UI.text "X-treme DANGER level"


        flagButton <- UI.button # set UI.text "Flag"

        contentGrid <- grid [[ element midCanvas], 
                             [ element instrText],
                             [ element ezGameButton, element medGameButton, element hardGameButton]]
                             # set UI.style[("text-align", "center"),
                                            ("align-items", "center")]


        gameStateRef <- liftIO $ newIORef gameState
        infoRef <- liftIO $ newIORef instrText


        getBody w #+ [ element contentGrid] 


        on UI.mousedown midCanvas $ \coord ->
            do curGameState <- liftIO( readIORef gameStateRef)
               if (flagFlag curGameState)
               then do liftIO $ modifyIORef gameStateRef $ updateStateBoardCoordFlag (doubleToCoord coord) 
                       curGameState <- liftIO( readIORef gameStateRef)
                       drawBoard midCanvas (0,0) ( (board curGameState))
                       element instrText # set UI.text (checkForWin (board curGameState))
                       return ()
               else do 
                       liftIO $ modifyIORef gameStateRef $ updateStateBoardCoord' (doubleToCoord coord) 
                       curGameState <- liftIO( readIORef gameStateRef)
                       drawBoard midCanvas (0,0) ( (board curGameState))
                       element instrText # set UI.text (checkForWin (board curGameState))
                       return ()


        on UI.click flagButton $ \_ ->
            do liftIO $ modifyIORef gameStateRef $ updateStateFlagging
               curGameState <- liftIO( readIORef gameStateRef)
               if (flagFlag curGameState)
               then do element flagButton # set UI.text "Mark blanks"
               else do element flagButton # set UI.text "Flag"
               

        on UI.click ezGameButton $ \event ->
            do element midCanvas # set UI.width ((double2Int (tileSize + tileSpaceSize))*8)
               element midCanvas # set UI.height ((double2Int (tileSize + tileSpaceSize))*8)


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

        on UI.click medGameButton $ \event ->
            do element midCanvas # set UI.width ((double2Int (tileSize + tileSpaceSize))*16)
               element midCanvas # set UI.height ((double2Int (tileSize + tileSpaceSize))*16)
               gameState <- liftIO newMinesweeper                                         
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
               getBody w #+ [ element newContentGrid] 

        
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

