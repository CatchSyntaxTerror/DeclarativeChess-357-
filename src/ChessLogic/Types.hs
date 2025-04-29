module ChessLogic.Types where

data Piece = Empty | Pawn Color (Int,Int) | Knight Color (Int,Int)
                   | Bishop Color (Int,Int) | Rook Color (Int,Int)
                   | King Color (Int,Int)


data Color = White | Black

data Board = PieceArr [[Piece]]