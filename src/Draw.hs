module Draw where
import Data.Matrix
import Type
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core (str, clickable)
import Brick.Types (Widget)
import Brick (AttrName, AttrMap, App(..), withBorderStyle,
 vBox, hBox, attrName, attrMap, withAttr,
 on)
import qualified Graphics.Vty.Attributes as V
import Lens.Micro ((^.))



-- >>> 

drawUi :: Game -> [Widget Name]
drawUi g = [drawGrid g]

drawBoard :: [Widget ()]
drawBoard = undefined

--drawChess :: Board -> [Widget ()]
--drawChess = undefined

drawMessageBox :: Game -> Widget ()
drawMessageBox = undefined

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Board")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [boardHeight, boardHeight-1..1]]
    cellsInRow y = [drawChess (getElem x y b) x y| x <- [1..boardWidth]]
    b = _board g

drawChess :: Chess -> Int -> Int -> Widget Name
drawChess Black x y =  withAttr blackChessAttr $ clickable (ChessName (x,y)) nonKingWidge
drawChess White x y =  withAttr whiteChessAttr $ clickable (ChessName (x,y)) nonKingWidge
drawChess Empty x y =  withAttr emptyChessAttr  nonKingWidge
drawChess King  x y =  withAttr kingChessAttr  $ clickable (ChessName (x,y)) kingWidge

-- cordToName :: Int -> Int -> Name
-- cordToName x y = show x ++ show y

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blackChessAttr, V.black `on` V.black)
  , (whiteChessAttr, V.white `on` V.white)
  , (kingChessAttr,  V.white `on` V.white)
  , (emptyChessAttr, V.brightYellow `on` V.brightYellow)
  ]


nonKingWidge :: Widget Name
nonKingWidge = str " "

kingWidge :: Widget Name
kingWidge = str "K"

blackChessAttr, whiteChessAttr, emptyChessAttr, kingChessAttr :: AttrName
blackChessAttr     = attrName "blackAttr"
whiteChessAttr     = attrName "whiteAttr"
emptyChessAttr     = attrName "emptyAttr"
kingChessAttr      = attrName "kingChessAttr"