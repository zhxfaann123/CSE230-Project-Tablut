module Event where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent(..), Next, EventM, Location)
import Brick.Widgets.Edit as E
import Graphics.Vty (Button)
import qualified Graphics.Vty as V
import Type
import Board
import Lens.Micro ((.~), (&))
import Lens.Micro.TH (makeLenses)
import Utils
import Data.Matrix

appEvent :: Game -> BrickEvent Name e -> EventM Name (Next Game)
-- appEvent s (MouseDown (ChessName (x,y)) Button modifiers loc) = continue initGame
appEvent s (VtyEvent (V.EvKey (V.KChar '1') [])) = continue $ modifyGameMessage s
-- appEvent _ (VtyEvent (V.EvMouseDown buttonName V.BLeft [] _))   = halt s
appEvent s (MouseDown (ChessCord cord) _ _ loc) = continue $ handleClickChess s cord
appEvent s (MouseDown ButtonRestart _ _ loc) = continue $ initGame
appEvent s (MouseUp _ _ _) = continue s 
appEvent s (VtyEvent (V.EvKey V.KEsc []))= halt s
appEvent s _ = halt s


handleClickChess :: Game -> Cord -> Game
handleClickChess s cord = if      not chessSelected && emptyChess     then s
                                 --  You select an emptychess and no chess has been selected 
                                 else if not chessSelected && not emptyChess then handleSelectChess s cord
                                 --  You select a non-empty chess
                                 else if chessSelected     && emptyChess     then handleTryMove s cord
                                 --  You try to move a selected chess to target cell
                                 else if chessSelected     && not emptyChess then handleSelectNewChess s cord
                                 --  Reselect the chess                           
                                 else error "error"
                                 --  You select a non-empty chess
                                    where chessSelected = (_selected s /= Nothing)
                                          emptyChess    = getChessFromGame s cord  == Empty

handleSelectChess :: Game -> Cord -> Game
handleSelectChess s cord = Game {
                        _interface = _interface s
                        , _turn      = _turn s
                        , _over      = _over s
                        , _winner    = _winner s
                        , _AI        = _AI s
                        , _board     = _board s
                        , _selected  = new_select
                        , _info      = new_info   
                  }
                  where new_select = if chessOfTurn s cord then Just cord else Nothing
                        new_info   = if chessOfTurn s cord then Just "New Chess Selected" else Just "You cannot Select the chess of your enemy"
                        --chess      = getChessFromGame s cord
                        -- cord       = mapStringToCord str

-- Try move the selected chess to the cell.
handleTryMove :: Game -> Cord -> Game
handleTryMove s target_cord = Game {
                        _interface = _interface s
                        , _turn      = new_turn
                        , _over      = new_over
                        , _winner    = new_winner
                        , _AI        = _AI s
                        , _board     = new_board
                        , _selected  = Nothing
                        , _info      = new_info   
                  }
                  where new_board  = if isLegalMove move old_board then makeMove move old_board else old_board
                        new_over   = fst $ isOver new_board
                        new_winner = if new_over then Just (snd $ isOver new_board) else Nothing
                        new_turn   = if old_turn == P_Black then P_White else P_Black
                        new_info   = Just "nothing"
                        old_turn   = _turn s
                        old_board  = _board s 
                        Just selected_cord = _selected s
                        move       = (selected_cord, target_cord)

                        
-- If the chess belongs to the turn of the player, select this chess. otherwise 
handleSelectNewChess :: Game -> Cord -> Game
handleSelectNewChess s cord = if chessOfTurn s cord then new_s_Select else new_s_nonSelect
                        where new_s_Select = Game {
                                                _interface = _interface s
                                                , _turn      = _turn s
                                                , _over      = _over s
                                                , _winner    = _winner s
                                                , _AI        = _AI s
                                                , _board     = _board s
                                                , _selected  = Just cord
                                                , _info      = _info s   
                                                }
                              new_s_nonSelect = Game {
                                                _interface = _interface s
                                                , _turn      = _turn s
                                                , _over      = _over s
                                                , _winner    = _winner s
                                                , _AI        = _AI s
                                                , _board     = _board s
                                                , _selected  = Nothing
                                                , _info      = _info s  
                                                }                  


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

