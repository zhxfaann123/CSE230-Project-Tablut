module Event where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent(..), Next, EventM, Location)
import Brick.Widgets.Edit as E
import Graphics.Vty (Button)
import qualified Graphics.Vty as V
import Type
import Lens.Micro ((.~), (&))

appEvent :: Game -> BrickEvent Name e -> EventM Name (Next Game)
-- appEvent s (MouseDown (ChessName (x,y)) Button modifiers loc) = continue initGame
appEvent s (VtyEvent (V.EvKey (V.KChar '1') [])) = continue $ modifyGameMessage s
appEvent _ (VtyEvent (V.EvKey (V.KChar '2') [])) = continue initGame_2
-- appEvent _ (VtyEvent (V.EvMouseDown buttonName V.BLeft [] _))   = halt s
appEvent s (MouseDown Button _ _ loc) = continue $ modifyGameMessage s
appEvent s (MouseUp Button (Just V.BLeft) _) = continue $ handleClick Button s 
appEvent s (VtyEvent (V.EvKey V.KEsc []))= halt s
appEvent s _ = halt s

modifyGameMessage :: Game -> Game
modifyGameMessage s = Game { 
      _interface = Game_Page
    , _turn      = P_White
    , _over      = False
    , _winner    = Nothing
    , _AI        = False
    , _board     = boardStart
    , _selected  = Nothing
    , _info      = Just "clicked"
    }  
                            

handleClick :: Name -> Game -> Game
handleClick _ s = initGame_2