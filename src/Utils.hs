module Utils where

import Type
import Data.Matrix

--mapNameToCord :: Name -> Cord
--mapNameToCord  (ChessName s)  = (x,y)
--    where x = (read s) - 10*y 
--         y = (read s) `mod` 10
--mapNameToCord _   = error "No map from button to cord"

--mapStringToCord :: String -> Cord
--mapStringToCord  s  = (x,y)
--    where x = (read s) - 10*y 
--          y = (read s) `mod` 10

getChessFromBoard :: Board -> Cord -> Chess
getChessFromBoard b c = getElem (fst c) (snd c) b

getChessFromGame :: Game -> Cord -> Chess
getChessFromGame s c = getElem (fst c) (snd c) (_board s)

chessOfTurn :: Game -> Cord -> Bool
chessOfTurn s c = if _turn s == P_White && (chess == White || chess == King) then True 
                  else if _turn s == P_Black && (chess == Black) then True
                  else False
                  where chess = getChessFromGame s c

chessToPlayer :: Chess -> Player
chessToPlayer Empty  = error "nonono"
chessToPlayer Black  = P_Black
chessToPlayer White  = P_White 