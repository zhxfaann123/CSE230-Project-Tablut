module Event where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent(..), Next, EventM)
import qualified Graphics.Vty as V
import Type

appEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
appEvent _ (VtyEvent (V.EvKey (V.KChar '1') [])) = continue initGame
appEvent _ (VtyEvent (V.EvKey (V.KChar '2') [])) = continue initGame
appEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt initGame
appEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
appEvent s _ = continue s