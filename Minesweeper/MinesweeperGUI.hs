module MinesweeperGUI where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Canvas
import Data.IORef
import Board

main :: IO ()
main = startGUI defaultConfig boardSuite

boardSuite :: Window -> UI ()
boardSuite w = 
    do  return w # set UI.title "~Minesweeper~"
        getBody w # set style [("background-color", "orange")]

        midCanvas <- UI.canvas # set UI.height 400
                               # set UI.width  400
                               # set style [("background-color", "white")]
        instrText <- string "helpful text"
        startGameButton <- UI.button # set UI.text "Start game"
        --on UI.click startGameButton $ \event ->
        --    do liftIO $ print "sorry, no game implemented"
        contentGrid <- grid [[ element midCanvas], 
                             [ element instrText],
                             [ element startGameButton]]
                             # set UI.style[("text-align", "center")]
        getBody w #+ [ element contentGrid
                      ]          -- add this list of stuff as children to this window
                      # set UI.style [("text-align", "center")]

       -- minesweeperStateRef <- liftIO $ newIORef
        drawBoard midCanvas (0,0) example
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
    do drawBox canvas p
       
       
drawSpace canvas p Space{item = Bomb}    = drawBox canvas p
drawSpace canvas p Space{item = Blank}    = drawBox canvas p
drawSpace canvas p Space{item = Numeric i}    = drawBox canvas p

drawBox :: UI.Canvas -> Point -> UI ()
drawBox canvas p =
    do canvas # set' UI.fillStyle (UI.htmlColor "teal")
       canvas # UI.fillRect p 50 50
       
       

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
