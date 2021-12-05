module Board where

import Type
import Data.Matrix
import Utils
import Prelude hiding (Right, Left)

data Direction = Left | Right | Up | Down | InValid deriving(Eq, Show)

type Over = (Bool, Player)

--instance Monad Board where
--       (>>=) :: Board a -> (a -> Board a) -> Board a
--       a  >>= process = process a
--       return :: Matrix Chess -> Board
--       return a = BoardC a

-- Assuming the move is legal
makeMove :: Move -> Board -> Board
makeMove m@(from@(x1, y1), to@(x2, y2)) b = removeCaptured to
       $ setElem chess to 
       $ setElem Empty from 
       $ b
       where chess = getChessFromBoard b from

-- Given the coordinate, remove the captured chesses as the consequence of the move.
removeCaptured :: Cord -> Board -> Board
removeCaptured c@(x,y) b = removeListCord b cordListToRemove
       where cordListToRemove =  [(n, isCaptured b n) | n <- allNeighbourCord] 
             -- chess            =  getElem x y b 
             cell             =  cellType c
             allNeighbourCord =  getNeighbourCord c

-- Remove the Chess at Cord from the board, iff the bool value associated with Cord is True.
removeListCord :: Board -> [(Cord, Bool)] -> Board
removeListCord b [] = b
removeListCord b ((c, True):xs)  =  removeListCord b' xs where b' = setElem Empty c b
removeListCord b ((c, False):xs) =  removeListCord b  xs

-- Judge a chess is captured
-- case1 : non-King chess is captured by two hositile chesses
isCaptured :: Board -> Cord -> Bool
isCaptured b c = case chess of Empty -> False
                               King -> kingsCondition
                               otherwise -> case1 || case2
       where  case1 = isEnclosed chess b c                     -- Two Opposide sides of enemy
              case2 = isEnclosedByThrone chess b c && isThroneEmpty
              isThroneEmpty = getElem 5 5 b == Empty
              chess = getChessFromBoard b c
              cell = cellType c              
              kingsCondition = case cell of Throne      -> getElem 4 5 b == Black && getElem 6 5 b == Black && getElem 5 4 b == Black && getElem 5 6 b == Black
                                            UpThrone    -> getElem 4 4 b == Black && getElem 4 6 b == Black && getElem 3 5 b == Black
                                            DownThrone  -> getElem 6 4 b == Black && getElem 6 6 b == Black && getElem 7 5 b == Black
                                            RightThrone -> getElem 4 6 b == Black && getElem 6 6 b == Black && getElem 5 7 b == Black
                                            LeftThrone  -> getElem 4 4 b == Black && getElem 5 3 b == Black && getElem 6 4 b == Black
                                            otherwise   -> isEnclosed King b c 

isKingOnThrone :: Board -> Bool
isKingOnThrone b = getElem 5 5 b == King

isThroneEmpty :: Board -> Bool
isThroneEmpty b = getElem 5 5 b == Empty

isEnemy :: Chess -> Chess -> Bool
isEnemy Black White = True
isEnemy Black King  = True
isEnemy White Black = True
isEnemy King  Black  = True
isEnemy _     _ = False

-- True if the the chess is at the sideThrone and there is an enemy chess on the opposide side of the Throne
-- Note: For this chess to be captured, the center throne is required to be empty
isEnclosedByThrone :: Chess -> Board -> Cord -> Bool
isEnclosedByThrone chess b c@(x,y) = upThrone || downThrone || leftThrone || rightThrone
                            where  upThrone    = x == 6 && y == 5 && isEnemy chess upChess
                                   downThrone  = x == 4 && y == 5 && isEnemy chess downChess
                                   leftThrone  = x == 5 && y == 4 && isEnemy chess leftChess
                                   rightThrone = x == 5 && y == 6 && isEnemy chess rightChess
                                   upChess    = getElem (x-1) y b
                                   downChess  = getElem (x+1) y b
                                   leftChess  = getElem x (y-1) b
                                   rightChess = getElem x (y+1) b                               


-- True if the chess in enclosed by two enemy
-- Note: Since the empty case is ruled out by `isCaptured`, we consider `Black` and `White/King` cases only here 
isEnclosed :: Chess -> Board -> Cord -> Bool
isEnclosed chess b c@(x,y) = if c `elem` corner   then False
                             else if x == 1 || x == 9  then verticalEnclosed
                             else if y == 1 || y == 9  then horiztalEnclosed
                             else                 verticalEnclosed || horiztalEnclosed
                     where corner           = [(1,1), (1,9), (9,1), (9,9)]
                           verticalEnclosed = upChessWhite    && downChessWhite
                           horiztalEnclosed = rightChessWhite && leftChessWhite
                           upChessWhite     = isEnemy chess $ getElem x (y+1) b
                           downChessWhite   = isEnemy chess $ getElem x (y-1) b
                           rightChessWhite  = isEnemy chess $ getElem (x-1) y b
                           leftChessWhite   = isEnemy chess $ getElem (x+1) y b

-- isEnclosed Black b c = foldr (\x -> if x == White then (+1) else (+0)) 0 $ getNeighbour c b
-- isEnclosed _     b c = foldr (\x -> if x == Black then (+1) else (+0)) 0 $ getNeighbour c b


-- >>> foldr (\x -> if x == White then (+1) else (+0)) 0 $ [White, White]
-- 2
--

-- Return a list of chess that is the neighboor of the given coordinate
getNeighbour :: Cord -> Board -> [Chess]
getNeighbour (1,1) b = [getElem 1 2 b, getElem 2 1 b]
getNeighbour (9,1) b = [getElem 8 1 b, getElem 9 2 b]
getNeighbour (1,9) b = [getElem 1 8 b, getElem 2 9 b]
getNeighbour (9,9) b = [getElem 9 8 b, getElem 8 9 b]
getNeighbour (1,y) b = [getElem 1 y b, getElem 1 (y+1) b, getElem 1 (y-1) b]
getNeighbour (9,y) b = [getElem 9 y b, getElem 9 (y+1) b, getElem 9 (y-1) b]
getNeighbour (x,1) b = [getElem x 1 b, getElem (x+1) 1 b, getElem (x-1) 1 b]
getNeighbour (x,9) b = [getElem x 9 b, getElem (x+1) 9 b, getElem (x-1) 9 b]
getNeighbour (x,y) b = [getElem (x+1) y b, getElem (x-1) y b, getElem x (y+1) b, getElem x (y-1) b]

getNeighbourCord :: Cord -> [Cord]
getNeighbourCord c@(1,1) = [(1,2), (2,1)]
getNeighbourCord c@(9,1) = [(8,1), (9,2)]
getNeighbourCord c@(1,9) = [(1,8), (2,9)]
getNeighbourCord c@(9,9) = [(9,8), (8,9)]
getNeighbourCord c@(1,y) = [(1,y), (1,y+1), (1,y-1)]
getNeighbourCord c@(9,y) = [(9,y), (9,y+1), (9,y-1)]
getNeighbourCord c@(x,1) = [(x,1), (x+1,1), (x-1,1)]
getNeighbourCord c@(x,9) = [(x,9), (x+1,9), (x-1,9)]
getNeighbourCord c@(x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
-- >>> (==Empty) <$> [Empty,Empty,King]
-- [True,True,False]
--

-- Check if the move is legal (orthogonally and no other chess is on the way)
-- No chess other than king could move to the throne
isLegalMove :: Move -> Board -> Bool
isLegalMove m@((from_x,from_y), (to_x, to_y)) board = (not (notKing && goThrone)) && 
                                              case d of InValid -> False
                                                        Up      -> foldr (&&) True cellList1
                                                        Down    -> foldr (&&) True cellList2
                                                        Left    -> foldr (&&) True cellList3
                                                        Right   -> foldr (&&) True cellList4 
                where  d         = getMoveDirection m
                       cellList1 = (==Empty) <$> toList (submatrix to_x (from_x-1) to_y from_y board) 
                       cellList2 = (==Empty) <$> toList (submatrix (from_x+1) to_x to_y from_y board) 
                       cellList3 = (==Empty) <$> toList (submatrix from_x to_x to_y (from_y-1) board) 
                       cellList4 = (==Empty) <$> toList (submatrix from_x to_x (from_y+1) to_y board) 
                       notKing   = getElem from_x from_y board /= King
                       goThrone  = to_x == 5 && to_y == 5

-- Judge the move direction by the given move
getMoveDirection :: Move -> Direction
getMoveDirection ((from_x, from_y), (to_x, to_y)) = if      from_x == to_x && from_y >  to_y then Left
                                                    else if from_x == to_x && from_y <  to_y then Right
                                                    else if from_x <  to_x && from_y == to_y then Down
                                                    else if from_x >  to_x && from_y == to_y then Up
                                                    else InValid

-- >>> getMoveDirection ((4,5),(4,4))
-- Right
--

-- >>> getMoveDirection ((1,1),(2,1))
-- Down
--

-- Judge if the game is over (the king captured or escaped)
isOver :: Board -> Over
isOver b = if      kingCaptured then (True, P_Black)
           else if kingEscaped  then (True, P_White)
           else if blackAllDied then (True, P_White)
           else    (False, P_Black)
       where kingCaptured = not $ foldr (||) False $ (==King) <$> toList b
             kingEscaped  = foldr (||) False $ (==King) <$> boardEdge b
             blackAllDied = foldr (&&) True  $ (==Black) <$> toList b
-- get all chesses that lie on the edge of the board (to check if the king escaped)
boardEdge :: Board -> [Chess]
boardEdge b = r1 ++ r9 ++ c1 ++ c9
       where r1 = [getElem 1 x b| x <- [1,2..9]]
             r9 = [getElem 9 x b| x <- [1,2..9]] 
             c1 = [getElem x 1 b| x <- [1,2..9]] 
             c9 = [getElem x 9 b| x <- [1,2..9]] 
-- ++ getCol 9 b ++ getRow 1 b ++ getRow 9 b

isBlack :: Chess -> Bool
isBlack Black = True
isBlack _     = False

isThrone :: Cord -> Bool
isThrone = undefined


---- Abandoned ----
--isCaptured :: Chess -> Board -> Cord -> Bool
--isCaptured Empty  _ _ = False
--isCaptured King  b c = case cell of Throne      -> getElem 4 5 b == Black && getElem 6 5 b == Black && getElem 5 4 b == Black && getElem 5 6 b == Black
--                                    UpThrone    -> getElem 4 4 b == Black && getElem 4 6 b == Black && getElem 3 5 b == Black
--                                    DownThrone  -> getElem 6 4 b == Black && getElem 6 6 b == Black && getElem 7 5 b == Black
--                                    RightThrone -> getElem 4 6 b == Black && getElem 6 6 b == Black && getElem 5 7 b == Black
--                                    LeftThrone  -> getElem 4 4 b == Black && getElem 5 3 b == Black && getElem 6 4 b == Black
--                                    otherwise   -> isEnclosed King b c 
--                     where cell = cellType c 
--isCaptured chess     b c = case1 || case2
--                     where   case1 = isEnclosed chess b c                     -- Two Opposide sides of enemy
--                             case2 = isEnclosedByThrone chess b c && isThroneEmpty
--                             isThroneEmpty = getElem 5 5 b == Empty

