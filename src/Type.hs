module Type where
import Data.Matrix
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~), (%~))

-- The state of the game. 
data Game = Game
    {
      _interface :: InterfaceType
    , _turn      :: Player
    , _over      :: Bool
    , _winner    :: Maybe Player
    , _AI        :: Bool
    , _board     :: Board
    , _selected  :: Maybe Cord
    , _info      :: Maybe String
    } 


--  The coordinate of the board, starting from 1(left-most/bottom).  
type Cord = (Int, Int)

--  The move from one place to another
type Move = (Cord, Cord)

--  The 3 kinds of chess plus an empty kind
data Chess = Black | White | Empty | King deriving (Show, Eq)

--  The board is represented by a matrix of which the element is Chess
type Board = Matrix Chess

 -- instance Monad Board where
  
--  The two layer of interface
data InterfaceType = Main_Page  | Game_Page

--  The two players
data Player   = P_Black | P_White


--  The types of name
-- data Name = ButtonName String | ChessName (Int, Int)
data Name = Button deriving(Ord, Show, Eq)

--  all kinds of Cell
data Cell = Throne | NonThrone | UpThrone | DownThrone | RightThrone | LeftThrone | OutOfBoard

cellType :: Cord -> Cell
cellType (5,5)   = Throne
cellType (5,6)   = RightThrone
cellType (5,4)   = LeftThrone
cellType (4,5)   = UpThrone
cellType (6,5)   = DownThrone
cellType (x,y)      = if x >= 1 && x <= 9 && y >= 1 && y <= 9 then NonThrone 
                      else OutOfBoard

sideThroneList :: [Cord]
sideThroneList = [(5,4), (5,6), (4,5), (6,5)]

boardHeight :: Int
boardHeight = 9

boardWidth :: Int
boardWidth = 9

--  The initial state of the board
boardStart :: Board
boardStart = fromLists [[Empty, Empty, Empty, Black, Black, Black, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Empty, Black, Empty, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Empty, White, Empty, Empty, Empty, Empty]
                       ,[Black, Empty, Empty, Empty, White, Empty, Empty, Empty, Black]
                       ,[Black, Black, White, White,  King, White, White, Black, Black]
                       ,[Black, Empty, Empty, Empty, White, Empty, Empty, Empty, Black]
                       ,[Empty, Empty, Empty, Empty, White, Empty, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Empty, Black, Empty, Empty, Empty, Empty]
                       ,[Empty, Empty, Empty, Black, Black, Black, Empty, Empty, Empty]
                       ]

boardStart_2 :: Board
boardStart_2 = fromLists [[Empty, Empty, Empty, Black, Black, Black, Empty, Empty, Empty]
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
-- │ Empty Empty Empty Empty White Empty Empty Empty Empty │
-- │ Black Empty Empty Empty White Empty Empty Empty Black │
-- │ Black Black White White  King White White Black Black │
-- │ Black Empty Empty Empty White Empty Empty Empty Black │
-- │ Empty Empty Empty Empty White Empty Empty Empty Empty │
-- │ Empty Empty Empty Empty Black Empty Empty Empty Empty │
-- │ Empty Empty Empty Black Black Black Empty Empty Empty │
-- └                                                       ┘
--

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
    , _selected  = Nothing
    , _info      = Nothing
    }     

initGame_2 :: Game
initGame_2 = Game    
    {
      _interface = Game_Page
    , _turn      = P_White
    , _over      = False
    , _winner    = Nothing
    , _AI        = False
    , _board     = boardStart_2
    , _selected  = Nothing
    , _info      = Nothing
    }     
