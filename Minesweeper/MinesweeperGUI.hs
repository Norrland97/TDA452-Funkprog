module MinesweeperGUI where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import Data.IORef

main :: IO ()
main = startGUI defaultConfig boardSuite

boardSuite :: Window -> UI ()
boardSuite w = 
    do  return w # set UI.title "~Minesweeper~"
        instrText <- string "helpful text"
        getBody w #+ [element instrText]-- add this list of stuff as children to this window
        getBody w # set style [("background-color", "orange")]
        return ()
