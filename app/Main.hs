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
