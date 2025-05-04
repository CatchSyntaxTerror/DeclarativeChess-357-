module ChessLogic.Types where

--These imports should be moved to chess functions once we get all the imports working
import Data.Char (digitToInt)
import Data.Char (intToDigit)

-- DATA

data Piece = Empty | Pawn Color   | Knight Color
                   | Bishop Color | Rook Color
                   | King Color   | Queen Color deriving Eq

data Color = White | Black | None deriving (Eq,Show)

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

-- Show Functions

showBoardWhite :: Board -> String
showBoardWhite (PieceArr pss) = foldr parseRow [] pss
    where
        parseRow row recur = recur ++ "[" ++ foldr parsePiece [] row ++  "]\n"
        parsePiece piece recur' = show piece ++ recur'

showBoardBlack :: Board -> String
showBoardBlack (PieceArr pss) = foldr parseRow [] pss
    where
        parseRow row recur = "[" ++ foldr parsePiece [] row ++  "]\n" ++ recur
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

-- Piece Logic

startCoord = 1
endCoord = 8

-- Takes a board, a piece, a coordinate, and returns that board with the piece placed at that coordinate
boardWithPiece :: Board -> Piece -> (Int,Int) -> Board
boardWithPiece (PieceArr pss) piece (x,y) = PieceArr (foldr replaceRow [] (zip pss [startCoord..]))
    where
        replaceRow (ps,row) recur = if x == row then foldr replaceCoordinate [] (zip ps [startCoord..]) : recur else ps : recur
        replaceCoordinate (p,col) recur = if y == col then piece : recur else p : recur


-- The main method for getting legal moves. Takes an FEN and returns a list of FENs
getLegalMoves :: String -> [String]
getLegalMoves = undefined

-- A helper method for getting candidate legal moves. Returns all moves that involve real piece
-- moves, but may leave the king in check and therefore may not be legal.
-- Legal moves require that a candidate move position does not lead to any candidate moves that 
-- involve the capture of the king
getCandidateMoves :: String -> [String]
getCandidateMoves = undefined

-- pattern match for different pieces
-- Board -> Piece Coordinate -> EnPCoordinate -> QCastling -> KCastling -> List of possible boards
getCandidateMovesForPiece :: Board -> (Int,Int) -> (Int,Int) -> Bool -> Bool -> [Board]
getCandidateMovesForPiece pss (row,col) = undefined

-- Board -> Piece Coordinate -> En Passant Coordinate
getCandidatePawn :: Board -> (Int,Int) -> (Int,Int) -> [Board]
getCandidatePawn = undefined

getCandidateKnight :: Board -> (Int,Int) -> [Board]
getCandidateKnight (PieceArr pss) (x,y) = boardWithCandidates
     where
         boardWithoutKnight = boardWithPiece (PieceArr pss) Empty (x,y) 
         boardWithCandidates = foldr (\square recur -> boardWithPiece boardWithoutKnight (Knight (colorSquare (PieceArr pss) (x,y))) square : recur) [] candidateSquares
         candidateSquares = filter (\p -> validSquare p && colorSquare (PieceArr pss) p /= colorSquare (PieceArr pss) (x,y)) squares
         squares = [(x + 2, y + 1),
                     (x + 2, y - 1),
                     (x + 1, y + 2),
                     (x + 1, y - 2),
                     (x - 1, y + 2),
                     (x - 1, y - 2),
                     (x - 2, y + 1),
                     (x - 2, y - 1)]


validSquare :: (Int,Int) -> Bool
validSquare (x,y) = x >= startCoord && x <= endCoord && y >= startCoord && y <= endCoord

emptySquare :: Board -> (Int,Int) -> Bool
emptySquare (PieceArr pss) (x,y) = (pss !! (x - startCoord)) !! (y - startCoord) == Empty

colorSquare :: Board -> (Int,Int) -> Color
colorSquare (PieceArr pss) (x,y) = getColor (pss !! (x - startCoord) !! (y - startCoord))
    where
        getColor Empty = None
        getColor (Pawn c) = c
        getColor (Knight c) = c
        getColor (Bishop c) = c
        getColor (Queen c) = c
        getColor (King c) = c
        getColor (Rook c) = c

getCandidateBishop :: Board -> (Int,Int) -> [Board]
getCandidateBishop = undefined

getCandidateRook :: Board -> (Int,Int) -> [Board]
getCandidateRook = undefined

getCandidateQueen :: Board -> (Int,Int) -> [Board]
getCandidateQueen = undefined

-- Board -> King Coordinate -> QueensideCastling -> KingsideCastling -> CandidateMoves
getCandidateKing :: Board -> (Int,Int) -> Bool -> Bool -> [Board]
getCandidateKing = undefined

-- This is defined in ChessFunctions but im using it here to test for GHCI

boardToFEN :: Board -> String
boardToFEN (PieceArr pss) = init (foldr parseBoard [] (reverse pss))
    where
        parseBoard row recur = parseRow 0 row ++ "/" ++ recur

        -- Handling each row of the board
        parseRow 0 [] = []
        parseRow numEmpty [] = [intToDigit numEmpty]
        parseRow numEmpty (Empty:ps) = parseRow (numEmpty + 1) ps
        parseRow 0 (p:ps) = showPiece p ++ parseRow 0 ps
        parseRow numEmpty row =  intToDigit numEmpty : parseRow 0 row

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

pieceArrFromBoard :: Board -> [[Piece]]
pieceArrFromBoard (PieceArr pss) = pss
