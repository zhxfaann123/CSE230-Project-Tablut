module Draw where
import Data.Matrix
import Type
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core (str, clickable, padLeftRight, padTopBottom, withDefAttr, translateBy, padBottom, (<=>))
import Brick.Types as T
import Brick (AttrName, AttrMap, App(..), withBorderStyle,
 vBox, hBox, attrName, attrMap, withAttr,
 on)
import qualified Graphics.Vty.Attributes as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)


drawUi :: Game -> [Widget Name]
drawUi g = [drawGrid g, infoLayer g, buttonLayer g]

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
    rows         = [hBox $ cellsInRow r | r <- [1,2..boardHeight]]
    cellsInRow x = [drawChess (getElem x y b) x y| y <- [1..boardWidth]]
    b = _board g

drawChess :: Chess -> Int -> Int -> Widget Name
drawChess Black x y =  clickable  (ChessCord (x,y)) $ withAttr blackChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChess White x y =  clickable  (ChessCord (x,y)) $ withAttr whiteChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChess Empty x y =  clickable  (ChessCord (x,y)) $ withAttr emptyChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChess King  x y =  clickable  (ChessCord (x,y)) $ withAttr kingChessAttr  $ padLeftRight 2 $ padTopBottom 1 $  kingWidge

-- cordToName :: Int -> Int -> Name
-- cordToName x y = show x ++ show y

buttonLayer :: Game -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (T.Pad 1) $ str "Click a button:") <=>
      C.hCenterLayer (vBox $ padTopBottom 1 <$> buttons) <=>
      C.hCenterLayer (padTopBottom 1 $ str "Or enter text and then click in this editor:")
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (ButtonAI,    "   AI   ",        attrName "button1"),
                       (ButtonRestart, "Restart"  , attrName "buttonRestart"),
                       (ButtonExit,  " Exit "     , attrName "buttonExit")
                     ]
        mkButton (name, label, attr) =
               clickable name $
               withDefAttr attr $
               B.border $
               padLeftRight 4 $
               padTopBottom 2 $ str label

-- >>> chessName (1,1)
-- "11"
--
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blackChessAttr, V.white `on` V.black)
  , (whiteChessAttr, V.black `on` V.white)
  , (kingChessAttr,  V.blue  `on` V.white)
  , (emptyChessAttr, V.blue  `on` V.yellow)
  , (attrName "info",      V.white `on` V.magenta)
  ]


nonKingWidge :: Int -> Int -> Widget Name
nonKingWidge x y = str $ show x ++ show y

kingWidge :: Widget Name
kingWidge = str "K"

blackChessAttr, whiteChessAttr, emptyChessAttr, kingChessAttr :: AttrName
blackChessAttr     = attrName "blackAttr"
whiteChessAttr     = attrName "whiteAttr"
emptyChessAttr     = attrName "emptyAttr"
kingChessAttr      = attrName "kingChessAttr"


infoLayer :: Game -> Widget Name
infoLayer game = T.Widget T.Fixed T.Fixed $ do
    c <- T.getContext
    let h = c^.T.availHeightL
        msg = case _info game of
                Nothing -> "nothing"
                Just sth -> show sth
    T.render $ translateBy (T.Location (0, h-1)) $ clickable ButtonInfo $
               withDefAttr (attrName "info") $
               C.hCenter (str ("Last reported click: " <> msg))