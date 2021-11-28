module Board where

import Type
import Data.Matrix


data Game = Game 
    {
      _interface :: InterfaceType
    , _turn      :: Player
    , _over      :: Bool
    , _winner    :: Player
    , _AI        :: Bool
    , _board     :: Board
    , _selected  :: Maybe Cord
    }



boardStart :: Board
boardStart = fromLists [[Empty, Empty, Empty, Black, Black, Black, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Empty, Black, Empty, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Black, White, Black, Empty, Empty, Empty]
                       ,[Black, Empty, Empty, Black, White, Black, Empty, Empty, Black]
                       ,[Black, Black, White, White,  King, White, White, Black, Black]
                       ,[Black, Empty, Empty, Empty, White, Empty, Empty, Empty, Black]
                       ,[Empty, Empty, Empty, Empty, White, Empty, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Empty, Black, Empty, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Black, Black, Black, Empty, Empty, Empty]
                       ]
-- >>> boardStart
-- ┌                                                       ┐
-- │ Empty Empty Empty Black Black Black Empty Empty Empty │
-- │ Empty Empty Empty Empty Black Empty Empty Empty Empty │
-- │ Empty Empty Empty Black White Black Empty Empty Empty │
-- │ Black Empty Empty Black White Black Empty Empty Black │
-- │ Black Black White White  King White White Black Black │
-- │ Black Empty Empty Empty White Empty Empty Empty Black │
-- │ Empty Empty Empty Empty White Empty Empty Empty Empty │
-- │ Empty Empty Empty Empty Black Empty Empty Empty Empty │
-- │ Empty Empty Empty Black Black Black Empty Empty Empty │
-- └                                                       ┘
--


move :: Move -> Game -> Game
move = undefined

isLegalMove :: Move -> Board -> Bool
isLegalMove = undefined

isOver :: Board -> Bool
isOver = undefined

isBlack :: Chess -> Bool
isBlack Black = True

isThrone :: Cord -> Bool
isThrone = undefined

