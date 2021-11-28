module Type where

import Data.Matrix
                   
 data Game = Game
    {
      _interface :: InterfaceType
    , _turn      :: Player
    , _over      :: Bool
    , _winner    :: Maybe Player
    , _AI        :: Bool
    , _board     :: Board
    , _selected  :: Maybe Cord
    } 

--  The coordinate of the board, starting from 1(left-most/bottom).  
type Cord = (Int, Int)

--  The move from one place to another
type Move = (Cord, Cord)

--  The 3 kinds of chess plus an empty kind
data Chess = Black | White | Empty | King deriving (Show)

--  The board is represented by a matrix of which the element is Chess
type Board = Matrix Chess

--  The two layer of interface
data InterfaceType = Main_Page  | Game_Page

--  The two players
data Player   = P_Black | P_While

--  The initial state of the board
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

                  

-- The initial state of the game
initGame :: Game
initGame = Game    
    {
      _interface = Game_Page
    , _turn      = P_White
    , _over      = False
    , _winner    = Nothing
    , _AI        = False
    , _board     = boardStart
    , _selected  = False
    }     
