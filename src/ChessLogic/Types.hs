module ChessLogic.Types where
    -- |Todo: pawn promotion, en passant, castling, check, checkmate, stalemate, draw, Bishops, Querens and rooks

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
testKing = boardFromFEN "8/8/3k4/8/8/4K3/8/8"
testRook = boardFromFEN "5k2/3p4/8/4r3/1P1R2P1/8/8/2K5"
testPawn = boardFromFEN "1n6/P1ppp3/1pk2p2/6pP/4Kp1P/1p1P2P1/1PPPPP2/8"
testCastleIllegal = boardFromFEN "1n3r2/3p1ppp/4kn2/2p5/8/4bNP1/PP2Q1PP/R1B1K2R"
ruyLopez = boardFromFEN "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R"
catalan = boardFromFEN "rnbqk2r/ppp1bppp/4pn2/3p4/2PP4/5NP1/PP2PPBP/RNBQK2R"
sveshnikov = boardFromFEN "r1bqkb1r/5p1p/p1np4/1p1Npp2/4P3/N7/PPP2PPP/R2QKB1R"
benko = boardFromFEN "1n3rk1/3pqppp/b4n2/2p5/8/4PN2/PP3PPP/R1BQK2R"
italian = boardFromFEN "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R"
pirc = boardFromFEN "rnbqk2r/ppp1ppbp/3p1np1/8/3PP3/2N1B3/PPPQ1PPP/R3KBNR"
amin = boardFromFEN "3k4/5n2/3p4/3Qb3/1p6/8/8/4K3"

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

-- Board -> Piece Coordinate -> En Passant Coordinate -> List of Boards
getCandidatePawn :: Board -> (Int,Int) -> (Int,Int) -> [Board]
getCandidatePawn (PieceArr pss) (x,y) (enPx,enPy) = boardWithEnPassant ++ boardWithCandidates ++ 
                                                    boardWithKnightPromotion ++ boardWithBishopPromotion ++
                                                    boardWithRookPromotion ++ boardWithQueenPromotion
    where
        boardWithoutEnPassant = boardWithPiece boardWithoutPawn Empty (enPx + ((-1) * colorDirection),enPy)
        boardWithoutPawn = boardWithPiece (PieceArr pss) Empty (x,y)
        -- Vector direction based on color
        colorDirection = if colorSquare (PieceArr pss) (x,y) == White then 1 else -1
        -- Determines if pawn is on starting rank based on color

        onStartingRank = (colorSquare (PieceArr pss) (x,y) == White && x == (startCoord + 1)) || (colorSquare (PieceArr pss) (x,y) == Black && x == (endCoord - 1))
        onSeventhRank = (colorSquare (PieceArr pss) (x,y) == White && x == (endCoord - 1)) || (colorSquare (PieceArr pss) (x,y) == Black && x == (startCoord + 1))

        pawnColor = colorSquare (PieceArr pss) (x,y)
        enemyColor = if colorSquare (PieceArr pss) (x,y) == White then Black else White

        checkForward n = if (validSquare (x + (n * colorDirection),y) && colorSquare (PieceArr pss) (x + (n * colorDirection), y) == None) then [(x + (n * colorDirection),y)] else []
        
        diagCandidateColor n = colorSquare (PieceArr pss) (x + colorDirection, y + n)
        checkDiags n = if ((validSquare (x + colorDirection,y + n)) && (diagCandidateColor n == enemyColor)) then [(x + colorDirection, y + n)] else []
        
        checkEnPassant n = if ((x + colorDirection, y + n) == (enPx,enPy)) then [(x + colorDirection, y + n)] else []

        candidateSquares
            | onSeventhRank = [] -- All movement of pawns on 7th rank results in promotion
            | onStartingRank = checkForward 1 ++ checkForward 2 ++ checkDiags 1 ++ checkDiags (-1)
            | otherwise = checkForward 1 ++ checkDiags 1 ++ checkDiags (-1)

        candidatePromotionSquares = if onSeventhRank then checkForward 1 ++ checkDiags 1 ++ checkDiags (-1) else []

        candidateEnPassantSquares = checkEnPassant 1 ++ checkEnPassant (-1)

        -- Regular pawn movement
        boardWithCandidates = foldr (\square recur -> boardWithPiece boardWithoutPawn (Pawn pawnColor) square : recur) [] candidateSquares
        -- En passant movement
        boardWithEnPassant = foldr (\square recur -> boardWithPiece boardWithoutEnPassant (Pawn pawnColor) square : recur) [] candidateEnPassantSquares
        
        -- Promotion Options
        boardWithKnightPromotion = foldr (\square recur -> boardWithPiece boardWithoutPawn (Knight pawnColor) square : recur) [] candidatePromotionSquares
        boardWithBishopPromotion = foldr (\square recur -> boardWithPiece boardWithoutPawn (Bishop pawnColor) square : recur) [] candidatePromotionSquares
        boardWithRookPromotion = foldr (\square recur -> boardWithPiece boardWithoutPawn (Rook pawnColor) square : recur) [] candidatePromotionSquares
        boardWithQueenPromotion = foldr (\square recur -> boardWithPiece boardWithoutPawn (Queen pawnColor) square : recur) [] candidatePromotionSquares

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
getCandidateBishop board pos@(x, y) = map (boardWithPiece clearedBoard bishopColor) validTargets
    where
        bishopColor = Bishop (colorSquare board pos)
        color = colorSquare board pos
        clearedBoard = boardWithPiece board Empty pos
        directions = [(1,1), (1,-1), (-1,1), (-1,-1)]
        validTargets = concatMap (getBishopMovesInDirection board color pos) directions

--Helper for getCandidateBishop
getBishopMovesInDirection :: Board -> Color -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
getBishopMovesInDirection board color (x, y) (dx, dy) = go (x + dx, y + dy)
    where
        go pos@(nx, ny)
            | not (validSquare pos) = []
            | emptySquare board pos = pos : go (nx + dx, ny + dy)
            | colorSquare board pos == color = []
            | otherwise = [pos]


getCandidateRook :: Board -> (Int,Int) -> [Board]
getCandidateRook (PieceArr pss) (x,y) = boardWithCandidates
    where
        boardWithCandidates = foldr (\square recur -> boardWithPiece boardWithoutRook (Rook (colorSquare (PieceArr pss) (x,y))) square : recur) [] candidateSquares
        boardWithoutRook = boardWithPiece (PieceArr pss) Empty (x,y)
        rColor = colorSquare (PieceArr pss) (x,y)
        -- the direction coordinates (1,0) (-1,0) (0,-1) (0,1) represent the rook directional movement.
        -- to make bishop or queen you can simply add the directions (1,1) (1,-1) (-1,1) (-1,-1)
        candidateSquares = candidatesInDirection (x,y) (1,0) rColor ++ candidatesInDirection (x,y) (-1,0) rColor
                        ++ candidatesInDirection (x,y) (0,-1) rColor ++ candidatesInDirection (x,y) (0,1) rColor
        candidatesInDirection (x,y) (dx,dy) rColor
            | not (validSquare (x + dx, y + dy)) = []
            | colorSquare (PieceArr pss) (x + dx, y + dy) == None = (x + dx, y + dy) : candidatesInDirection (x + dx, y + dy) (dx,dy) rColor
            | colorSquare (PieceArr pss) (x + dx, y + dy) /= rColor = [(x + dx, y + dy)]
            | colorSquare (PieceArr pss) (x + dx, y + dy) == rColor = []

getCandidateQueen :: Board -> (Int,Int) -> [Board]
getCandidateQueen board pos@(x, y) = map (boardWithPiece clearedBoard queenColor) validTargets
    where
        queenColor = Queen (colorSquare board pos)
        color = colorSquare board pos
        clearedBoard = boardWithPiece board Empty pos
        directions = [(1,1), (1,-1), (-1,1), (-1,-1), (1,0), (-1,0), (0,1), (0,-1)]
        validTargets = concatMap (getBishopMovesInDirection board color pos) directions

-- When generating enemy "threats" we shouldn't consider the king's castling
getCandidateKingWithoutCastles :: Board -> (Int,Int) -> [Board]
getCandidateKingWithoutCastles (PieceArr pss) (x,y) = boardWithCandidates
    where
        boardWithoutKing = boardWithPiece (PieceArr pss) Empty (x,y)
        boardWithCandidates = foldr (\square recur -> boardWithPiece boardWithoutKing (King (colorSquare (PieceArr pss) (x,y))) square : recur) [] candidateSquares
        candidateSquares = filter (\p -> validSquare p && colorSquare (PieceArr pss) p /= colorSquare (PieceArr pss) (x,y)) squares
        squares = [(x + 1, y + 1),
                     (x + 1, y - 1),
                     (x + 1, y),
                     (x, y - 1),
                     (x, y + 1),
                     (x - 1, y - 1),
                     (x - 1, y + 1),
                     (x - 1, y)]
        colorKing = colorSquare (PieceArr pss) (x,y)

-- Board -> King Coordinate -> QueensideCastling -> KingsideCastling -> EnemyCandidates -> CandidateMoves
getCandidateKing :: Board -> (Int,Int) -> Bool -> Bool -> [Board] -> [Board]
getCandidateKing (PieceArr pss) (x,y) kCas qCas enemyCandidates = boardWithCandidates ++ candidateKingside ++ candidateQueenside
     where
        boardWithoutKing = boardWithPiece (PieceArr pss) Empty (x,y)
        boardWithCandidates = foldr (\square recur -> boardWithPiece boardWithoutKing (King (colorSquare (PieceArr pss) (x,y))) square : recur) [] candidateSquares
        candidateSquares = filter (\p -> validSquare p && colorSquare (PieceArr pss) p /= colorSquare (PieceArr pss) (x,y)) squares
        squares = [(x + 1, y + 1),
                     (x + 1, y - 1),
                     (x + 1, y),
                     (x, y - 1),
                     (x, y + 1),
                     (x - 1, y - 1),
                     (x - 1, y + 1),
                     (x - 1, y)]
        colorKing = colorSquare (PieceArr pss) (x,y)

        checkSquaresForEmpty2 (x1,y1)(x2,y2) = colorSquare (PieceArr pss) (x1,y1) == None && 
                                               colorSquare (PieceArr pss) (x2,y2) == None

        checkSquaresForEmpty3 (x1,y1) (x2,y2) (x3,y3) = colorSquare (PieceArr pss) (x1,y1) == None && 
                                                        colorSquare (PieceArr pss) (x2,y2) == None &&
                                                        colorSquare (PieceArr pss) (x3,y3) == None

        checkSquaresForThreat (x1,y1) (x2,y2) (x3,y3) color = (foldr (\cand recur -> colorSquare cand (x1,x2) == color || 
                                                                 colorSquare cand (x2,y2) == color || 
                                                                 colorSquare cand (x3,y3) == color || recur) False enemyCandidates)
        castleKingside
            | colorKing == White = kCas && (checkSquaresForEmpty2 (1,6)(1,7)) && not (checkSquaresForThreat (1,5)(1,6)(1,7) Black)
            | colorKing == Black = kCas && (checkSquaresForEmpty2 (8,6)(8,7)) && not (checkSquaresForThreat (8,5)(8,6)(8,7) White)
        
        castleQueenside
            | colorKing == White = qCas && (checkSquaresForEmpty3 (1,4)(1,3)(1,2)) && not (checkSquaresForThreat (1,3)(1,4)(1,5) Black)
            | colorKing == Black = qCas && (checkSquaresForEmpty3 (8,4)(8,3)(8,2)) && not (checkSquaresForThreat (8,3)(8,4)(8,5) White)

        boardWithoutKingsideCastle 
            | colorKing == White = boardWithPiece boardWithoutKing Empty (1,8)
            | colorKing == Black = boardWithPiece boardWithoutKing Empty (8,8)

        boardWithoutQueensideCastle
            | colorKing == White = boardWithPiece boardWithoutKing Empty (1,1)
            | colorKing == Black = boardWithPiece boardWithoutKing Empty (8,1)

        boardWithKingsideCastle
            | colorKing == White = boardWithPiece (boardWithPiece boardWithoutKingsideCastle (King White) (1,7)) (Rook White) (1,6)
            | colorKing == Black = boardWithPiece (boardWithPiece boardWithoutKingsideCastle (King Black) (8,7)) (Rook Black) (8,6)

        boardWithQueensideCastle
            | colorKing == White = boardWithPiece (boardWithPiece boardWithoutQueensideCastle (King White) (1,3)) (Rook White) (1,4)
            | colorKing == Black = boardWithPiece (boardWithPiece boardWithoutQueensideCastle (King Black) (8,3)) (Rook Black) (8,4)
        
        candidateKingside = if castleKingside then [boardWithKingsideCastle] else []
        candidateQueenside = if castleQueenside then [boardWithQueensideCastle] else []

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
