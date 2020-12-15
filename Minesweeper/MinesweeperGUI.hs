module MinesweeperGUI where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
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
        on UI.click startGameButton $ \event ->
            do liftIO $ print "sorry, no game implemented"
        contentGrid <- grid [[ element midCanvas], 
                             [ element instrText],
                             [ element startGameButton]]
                             # set UI.style[("text-align", "center")]
        getBody w #+ [ element contentGrid
                      ]          -- add this list of stuff as children to this window
                      # set UI.style [("text-align", "center")]

       -- minesweeperStateRef <- liftIO $ newIORef
        drawSpace midCanvas (Space Bomb Hidden)
        return ()

drawBoard :: UI.Canvas -> Board -> UI ()
drawBoard canvas board = undefined


drawSpace :: UI.Canvas -> Space -> UI ()
drawSpace canvas Space{state = Hidden} = drawBox canvas
drawSpace canvas Space{item = Bomb}    = drawBox canvas

drawBox :: UI.Canvas -> UI ()
drawBox canvas =
    do canvas # set' UI.fillStyle (UI.htmlColor "teal")
       canvas # UI.fillRect (0,0) 50 50
       canvas # set' UI.fillStyle (UI.htmlColor "orange")
       {-}
       canvas # UI.beginPath
       canvas # UI.lineTo (30.0, 300.0)
       canvas # UI.closePath
       canvas # UI.fill
       canvas # UI.stroke-}
       {-}
       UI.beginPath canvas
       UI.moveTo (0, 50) canvas
       UI.moveTo (50,50) canvas
       UI.moveTo (50, 0) canvas
       UI.moveTo (0,  0) canvas
       UI.closePath canvas
       UI.stroke canvas-}
