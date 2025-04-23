module ChessData 
    ( Piece
    , Color
    , Coordinate
    , Board
    ) where

data Piece = Empty | Pawn Color Coordinate | Knight Color Coordinate
                   | Bishop Color Coordinate | Rook Color Coordinate
                   | King Color Coordinate


data Color = White | Black

data Coordinate = Coord Int Int

data Board = FEN String | PieceArr [[Piece]]