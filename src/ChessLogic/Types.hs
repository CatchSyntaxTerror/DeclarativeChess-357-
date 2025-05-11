module ChessLogic.Types where
    -- |Todo: pawn promotion, en passant, castling, check, checkmate, stalemate, draw, Bishops, Querens and rooks



-- DATA

data Piece = Empty | Pawn Color   | Knight Color
                   | Bishop Color | Rook Color
                   | King Color   | Queen Color deriving Eq

data Color = White | Black | None deriving (Eq,Show)

data Board = PieceArr [[Piece]] 

data Position = Position {
    board :: Board,
    move :: Color,
    wKCastle :: Bool,
    wQCastle :: Bool,
    bKCastle :: Bool,
    bQCastle :: Bool,
    enPassant :: (Int,Int),
    halfMove :: Int,
    fullMove :: Int
} deriving Show

-- SHOW

instance Show Piece where
    show = showPiece

instance Show Board where
    show = showBoardWhite

-- Show Functions

showBoardWhite :: Board -> String
showBoardWhite (PieceArr pss) = foldr parseRow [] pss
    where
        parseRow row recur = recur ++ "[" ++ foldr parsePiece [] row ++  "]\n"
        parsePiece piece recur' = show piece ++ recur'

showBoardBlack :: Board -> String
showBoardBlack (PieceArr pss) = foldr parseRow [] pss
    where
        parseRow row recur = "[" ++ foldr parsePiece [] row ++  "]/n" ++ recur
        parsePiece piece recur' = recur' ++ show piece

showPiece :: Piece -> String
showPiece Empty = " "
showPiece (Pawn White) = "P"
showPiece (Knight White) = "N"
showPiece (Bishop White) = "B"
showPiece (Rook White) = "R"
showPiece (Queen White) = "Q"
showPiece (King White) = "K"
showPiece (Pawn Black) = "p"
showPiece (Knight Black) = "n"
showPiece (Bishop Black) = "b"
showPiece (Rook Black) = "r"
showPiece (Queen Black) = "q"
showPiece (King Black) = "k"

charToPiece :: Char -> Piece
charToPiece 'P' = Pawn White
charToPiece 'N' = Knight White
charToPiece 'B' = Bishop White
charToPiece 'R' = Rook White
charToPiece 'Q' = Queen White
charToPiece 'K' = King White
charToPiece 'p' = Pawn Black
charToPiece 'n' = Knight Black
charToPiece 'b' = Bishop Black
charToPiece 'r' = Rook Black
charToPiece 'q' = Queen Black
charToPiece 'k' = King Black
charToPiece ' ' = Empty
charToPiece _ = Empty