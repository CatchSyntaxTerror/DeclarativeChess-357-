module ChessLogic.Types where
import Data.Char (digitToInt)

-- DATA

data Piece = Empty | Pawn Color   | Knight Color
                   | Bishop Color | Rook Color
                   | King Color   | Queen Color

data Color = White | Black

data Board = PieceArr [[Piece]]


-- SHOW

instance Show Piece where
    show = showPiece


instance Show Board where
    show = showBoardWhite

-- Starting position constructed as piece array for test
startingPosition = boardFromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

-- Some Test Positions
ruyLopez = boardFromFEN "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R"
catalan = boardFromFEN "rnbqk2r/ppp1bppp/4pn2/3p4/2PP4/5NP1/PP2PPBP/RNBQK2R"
sveshnikov = boardFromFEN "r1bqkb1r/5p1p/p1np4/1p1Npp2/4P3/N7/PPP2PPP/R2QKB1R"

showBoardWhite :: Board -> String
showBoardWhite (PieceArr pss) = foldr parseRow [] pss
    where
        parseRow row recur = recur ++ "[" ++ foldr parsePiece [] row ++  "]\n"
        parsePiece piece recur' = show piece ++ recur'

showBoardBlack :: Board -> String
showBoardBlack (PieceArr pss) = foldr parseRow [] pss
    where
        parseRow row recur = "[" ++ foldr parsePiece [] row ++  "]\n" ++ recur
        parsePiece piece recur' = show piece ++ recur'

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


-- This is defined in ChessFunctions but im using it here to test for GHCI

boardFromFEN :: String -> Board
boardFromFEN fen = PieceArr (reverse (foldr go [] (rowsFromFEN fen)))
    where
        go row recur = parseFENrow row : recur

rowsFromFEN :: String -> [String]
rowsFromFEN fen = getRow fen 0
    where
        getRow ('/':cs) n = getRow cs n
        getRow fen 7 = [fen]
        getRow fen n = takeWhile (/= '/') fen : getRow (dropWhile (/= '/') fen) (n + 1)

parseFENrow :: String -> [Piece]
parseFENrow strs = foldr go [] strs
   where
       go 'p' recur = Pawn   Black : recur
       go 'n' recur = Knight Black : recur
       go 'b' recur = Bishop Black : recur
       go 'r' recur = Rook   Black : recur
       go 'q' recur = Queen  Black : recur
       go 'k' recur = King   Black : recur
       go 'P' recur = Pawn   White : recur
       go 'N' recur = Knight White : recur
       go 'B' recur = Bishop White : recur
       go 'R' recur = Rook   White : recur
       go 'Q' recur = Queen  White : recur
       go 'K' recur = King   White : recur
       go n recur = replicate (digitToInt n) Empty ++ recur