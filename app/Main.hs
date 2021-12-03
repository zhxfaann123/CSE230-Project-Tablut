module Main where

import Type

import Lib
import Board
import Draw
import Event

import Data.List
import Data.Monoid

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Util
import Brick.AttrMap

import Graphics.Vty as V

ui :: Widget n
ui =
    vBox [ str "This text uses the global default attribute."
         , withAttr foundAttr $
           str "Specifying an attribute name means we look it up in the attribute tree."
         , (withAttr foundFgOnly $
           str ("When we find a value, we merge it with its parent in the attribute")
           <=> str "name tree all the way to the root (the global default).")
         , str "A missing attribute name just resumes the search at its parent."
         , withAttr (general2) $
           str "In this way we build complete attribute values by using an inheritance scheme."
         , withAttr foundAttr $
           str "You can override everything ..."
         , withAttr foundFgOnly $
           str "... or only what you want to change and inherit the rest."
         , str "Attribute names are assembled with the Monoid append operation to indicate"
         , str "hierarchy levels, e.g. \"window\" <> \"title\"."
         , str " "
         ]
         

foundAttr, foundFgOnly, general, general2 :: AttrName
foundAttr = attrName "foundFull"
foundFgOnly = attrName "foundFgOnly"
general = attrName "general"
general2 = attrName "general2"

app :: App Game e Name
app =
    App { appDraw = drawUi 
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

main :: IO Game
-- main = defaultMain app initGame
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    customMain initialVty buildVty Nothing app initGame
