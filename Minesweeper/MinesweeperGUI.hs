module MinesweeperGUI where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Canvas
import Data.IORef
import System.Random
import Minesweeper
import Board

main :: IO ()
main = startGUI defaultConfig boardSuite

data Minesweeper = MS { size  :: (Int,Int),
                        board :: Board,
                        views :: Views}
        deriving (Show)

newMinesweeper :: IO Minesweeper
newMinesweeper = do g <- newStdGen
                    let (i, _) = randomR (0, 5000) g
                    return (MS {size = (8,8), board = hideAll (makeBoard (mkStdGen i) 10 (8,8)), views = []})


setBoardSize :: Int -> Int -> UI ()
setBoardSize w h = UI.canvas # set UI.width w
                             # set UI.height h


boardSuite :: Window -> UI ()
boardSuite w = 
    do  return w # set UI.title "~Minesweeper~"
        getBody w # set style [("background-color", "orange")]
        -- TODO button styling

        -- buttons for selecting difficulty
        ezGameButton <- UI.button # set UI.text "Easy game"
        medGameButton <- UI.button # set UI.text "Medium difficulty"
        hardGameButton <- UI.button # set UI.text "X-treme"
        on UI.click ezGameButton $ \event ->
            do setBoardSize 480 480
        on UI.click medGameButton $ \event ->
            do setBoardSize 960 960
        on UI.click hardGameButton $ \event ->
            do setBoardSize 1800 960

        -- set properties of canvas
        midCanvas <- UI.canvas # set textAlign Center
                               # set textFont "35px sans-serif"
                               # set UI.style[("align-items", "center"),
                                              ("display", "flex")]
                              -- # set style [("background-color", "white")]

        -- text styling
        pure midCanvas # set textFont "35px sans-serif"
        pure midCanvas # set UI.strokeStyle "gray"
        pure midCanvas # set UI.fillStyle   (UI.htmlColor "black")
        pure midCanvas # set UI.textAlign Center

        instrText <- string "helpful text"
        --startGameButton <- UI.button # set UI.text "Start game"
        --on UI.click startGameButton $ \event ->
        --    do liftIO $ print "sorry, no game implemented"
        initContentGrid <- grid [[ element midCanvas], 
                                 [ element instrText],
                                 [ element ezGameButton, element medGameButton, element hardGameButton]]
                                  # set UI.style[("padding", "200px"),
                                                 ("text-align", "center"),
                                                 ("align-items", "center")]
        
        getBody w #+ [ element contentGrid]          -- add this list of stuff as children to this window
                     -- # set UI.textAlign Center
                      

       -- minesweeperStateRef <- liftIO $ newIORef
        gameLoop
        return ()

drawBoard :: UI.Canvas -> Point -> Board -> UI ()
drawBoard canvas (x,y) (r:rs) = 
    do drawRow canvas (x,y) r
       drawBoard canvas (x, y+70) rs
drawBoard canvas p [] =
    do return ()


drawRow :: UI.Canvas -> Point -> Row -> UI ()
drawRow canvas (x,y) (r:rs) =
    do drawSpace canvas (x,y) r
       drawRow canvas (x+70, y) rs
drawRow canvas p [] =
    do return ()


drawSpace :: UI.Canvas -> Point -> Space -> UI ()
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
          --    # set UI.textFont    "35px sans-serif"
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
