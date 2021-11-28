module Draw where

import Type
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str)

drawUi :: Game -> [Widget ()]
drawUi _ = [C.centerLayer $ str "try something"]