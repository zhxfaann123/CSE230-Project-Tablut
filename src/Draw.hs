module Draw where
import Data.Matrix
import Type
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core (str, clickable, padLeftRight, padTopBottom,
 withDefAttr, translateBy, padBottom, (<=>), setAvailableSize,
 padLeft, padRight, padTop, (<+>))
import Brick.Types as T
import Brick (AttrName, AttrMap, App(..), withBorderStyle,
 vBox, hBox, attrName, attrMap, withAttr,
 on, hLimit, vLimit)
import qualified Graphics.Vty.Attributes as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)


drawUi :: Game -> [Widget Name]
drawUi g =  [(drawGrid g) <+> (buttonLayer g)]

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
    cellsInRow x = case _over g of False -> [drawChess (getElem x y b) x y| y <- [1..boardWidth]]
                                   True  -> [drawChessStop (getElem x y b) x y| y <- [1..boardWidth]]
    b = _board g

drawChess :: Chess -> Int -> Int -> Widget Name
drawChess Black x y =  withBorderStyle BS.unicodeRounded $ B.border $ clickable  (ChessCord (x,y)) $ withAttr blackChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChess White x y =  withBorderStyle BS.unicodeRounded $ B.border $ clickable  (ChessCord (x,y)) $ withAttr whiteChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChess Empty x y =  withBorderStyle BS.unicodeRounded $ B.border $ clickable  (ChessCord (x,y)) $ withAttr emptyChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChess King  x y =  withBorderStyle BS.unicodeRounded $ B.border $ clickable  (ChessCord (x,y)) $ withAttr kingChessAttr  $ padLeftRight 2 $ padTopBottom 1 $  kingWidge

drawChessStop :: Chess -> Int -> Int -> Widget Name
drawChessStop Black x y =  B.border $ withAttr blackChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChessStop White x y =  B.border $ withAttr whiteChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChessStop Empty x y =  B.border $ withAttr emptyChessAttr $ padLeftRight 2 $ padTopBottom 1 $  nonKingWidge x y
drawChessStop King  x y =  B.border $ withAttr kingChessAttr  $ padLeftRight 2 $ padTopBottom 1 $  kingWidge
-- cordToName :: Int -> Int -> Name
-- cordToName x y = show x ++ show y

buttonLayer :: Game -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (T.Pad 1) $ str "Click a button:") <=>
      C.hCenterLayer (hBox $ padTopBottom 1 <$> buttons) <=>
      C.hCenterLayer (infoBox st)
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (ButtonAI,    "   AI  ",        attrName "button1"),
                       (ButtonRestart, "Restart"  , attrName "buttonRestart"),
                       (ButtonExit,  " Exit  "     , attrName "buttonExit")
                     ]
        mkButton (name, label, attr) =
               hLimit 18 $
               clickable name $
               withDefAttr attr $
               B.border $
               padLeftRight 4 $
               padTopBottom 1 $ str label

-- >>> chessName (1,1)
-- "11"
--
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blackChessAttr, V.white `on` V.black)
  , (whiteChessAttr, V.black `on` whiteColor)
  , (kingChessAttr,  V.blue  `on` whiteColor)
  , (emptyChessAttr, V.blue  `on` emptyColor)
  , (attrName "info", V.white `on` V.magenta)
  ]

whiteColor :: V.Color
whiteColor = V.rgbColor 255 255 255

emptyColor :: V.Color
emptyColor = V.rgbColor 255 153 51

nonKingWidge :: Int -> Int -> Widget Name
-- nonKingWidge x y = str $ show x ++ show y
nonKingWidge _ _ = str "  "

kingWidge :: Widget Name
kingWidge = str "K "

blackChessAttr, whiteChessAttr, emptyChessAttr, kingChessAttr :: AttrName
blackChessAttr     = attrName "blackAttr"
whiteChessAttr     = attrName "whiteAttr"
emptyChessAttr     = attrName "emptyAttr"
kingChessAttr      = attrName "kingChessAttr"


infoBox :: Game -> Widget Name
infoBox s =   
            withBorderStyle BS.unicodeBold
            $ B.borderWithLabel (str "Message Box")
            $ hLimit 50 
            $ vLimit 12 
            $ padRight  T.Max 
            $ padBottom T.Max 
            $ padTop    T.Max  
            $ padLeft   T.Max 
            $ C.center 
            $ str msg
              where msg = case _info s of Nothing -> "nothing"
                                          Just sth -> show sth

infoLayer :: Game -> Widget Name
infoLayer game = T.Widget T.Fixed T.Fixed $ do
    c <- T.getContext
    let h = c^.T.availHeightL
        msg = case _info game of
                Nothing -> "nothing"
                Just sth -> show sth
    T.render $ translateBy (T.Location (0, h-4)) $ clickable ButtonInfo $
               withDefAttr (attrName "info") $
               C.hCenter (str ("Last reported click: " <> msg))