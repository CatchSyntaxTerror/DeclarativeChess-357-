module ChessLogic.ChessFunctions where

import ChessLogic.Types
import ChessLogic.FENParse
import ChessLogic.ChessConstants
import Data.List(zip4)

-- Takes a board, a piece, a coordinate, and returns that board with the piece placed at that coordinate
boardWithPiece :: Board -> Piece -> (Int,Int) -> Board
boardWithPiece (PieceArr pss) piece (x,y) = PieceArr (foldr replaceRow [] (zip pss [startCoord..]))
    where
        replaceRow (ps,row) recur = if x == row then foldr replaceCoordinate [] (zip ps [startCoord..]) : recur else ps : recur
        replaceCoordinate (p,col) recur = if y == col then piece : recur else p : recur


-- The main method for getting legal moves. Takes an FEN and returns a list of FENs
getLegalMoves :: String -> [String]
getLegalMoves fen = map positionToFEN (getLegalFromPosition (positionFromFEN fen))

isLegalNewCoordinate :: String -> (Int,Int) -> (Int,Int) -> Bool
isLegalNewCoordinate fen coord coord2 = elem coord2 (getLegalSquaresForCoordinate fen coord)

-- Call This For MouseClicks
newPositionFromCoordinates :: String -> (Int,Int) -> (Int,Int) -> String
newPositionFromCoordinates fen before after = positionToFEN (newPositionFromPositionCoordinate (positionFromFEN fen) before after)

-- Takes a position, a selected square, a target square and returns the updated position if legal, otherwise the original position
newPositionFromPositionCoordinate :: Position -> (Int,Int) -> (Int,Int) -> Position
newPositionFromPositionCoordinate pos@(Position board _ _ _ _ _ _ _ _) before after = 
    if null outPosition then pos else head outPosition
    where
        outPosition = filter (\position2@(Position board2 _ _ _ _ _ _ _ _) -> 
            (isKing (getPieceMoved board board2) && getNewCoordinateKing board (boardFromPosition position2) == after)
            || (not (isKing(getPieceMoved board board2)) && getNewCoordinate board (boardFromPosition position2) == after)) (getLegalFromCoordinatePosition pos before)
        legalPositions = getLegalFromCoordinatePosition pos before

boardFromPosition :: Position -> Board
boardFromPosition (Position board _ _ _ _ _ _ _ _) = board

getLegalSquaresForCoordinate :: String -> (Int,Int) -> [(Int,Int)]
getLegalSquaresForCoordinate fen coord = getLegalSquaresPosition (positionFromFEN fen) coord

isKing :: Piece -> Bool
isKing (King _) = True
isKing _ = False

isPawn :: Piece -> Bool
isPawn (Pawn _) = True
isPawn _ = False

-- TODO: Make en passant highlight, make castles illegal when in check
getLegalSquaresPosition :: Position -> (Int,Int) -> [(Int,Int)]
getLegalSquaresPosition before@(Position board c _ _ _ _ _ _ _) coord =
    map (\(Position after _ _ _ _ _ _ _ _) -> 
        if isPawnBoard board coord
        then getNewCoordinatePawn board after
        else if isKingBoard board coord
        then getNewCoordinateKing board after
        else getNewCoordinate board after) pieceMoves
    where
        pieceMoves = filter (\(Position after _ _ _ _ _ _ _ _) -> 
            if isPawnBoard board coord
            then getCoordinateMovedPawn board after c == coord
            else if isKingBoard board coord
            then getCoordinateMovedKing board after == coord
            else getCoordinateMoved board after == coord) (getLegalFromPosition before)

getLegalMovesForCoordinate :: String -> (Int,Int) -> [String]
getLegalMovesForCoordinate fen coord = map positionToFEN (getLegalFromCoordinatePosition (positionFromFEN fen) coord)

isPawnBoard :: Board -> (Int,Int) -> Bool
isPawnBoard _ (9,9) = False
isPawnBoard (PieceArr pss) (row,col) = isPawn ((pss !! (row - startCoord)) !! (col - startCoord))

isKingBoard :: Board -> (Int,Int) -> Bool
isKingBoard _ (9,9) = False
isKingBoard (PieceArr pss) (row,col) = isKing ((pss !! (row - startCoord)) !! (col - startCoord))

getLegalFromCoordinatePosition :: Position -> (Int,Int) -> [Position]
getLegalFromCoordinatePosition before@(Position board c _ _ _ _ _ _ _) coord = 
    filter (\(Position after _ _ _ _ _ _ _ _) -> 
        if isPawnBoard board coord
        then getCoordinateMovedPawn board after c == coord
        else if isKingBoard board coord 
        then getCoordinateMovedKing board after == coord
        else getCoordinateMoved board after == coord) (getLegalFromPosition before)

getLegalFromPosition :: Position -> [Position]
getLegalFromPosition (Position board color wk wq bk bq enP hm fm) = foldr newPosition [] outBoards
    where
        newPosition bn recur = Position bn newColor (newWK bn) (newWQ bn) (newBK bn) (newBQ bn) (newEnP bn) (newHM bn) (newFM bn) : recur

        outBoards
            | color == White = getLegalMovesFromBoard board White enP wk wq
            | otherwise      = getLegalMovesFromBoard board Black enP bk bq
        kingMoved b = getPieceMoved board b == (King color)
        pawnMoved b = getPieceMoved board b == (Pawn color)
        kRookMoved b = getCoordinateMoved board b == if color == White then (startCoord,endCoord) else (endCoord,endCoord)
        qRookMoved b = getCoordinateMoved board b == if color == White then (startCoord,startCoord) else (endCoord,startCoord)

        newEnP b = getEnPassantSquare board b
        newColor = if color == White then Black else White
        -- TODO: Make Color Dependent
        newWK b = if color == White then wk && not (kingMoved b) && not (kRookMoved b) else wk
        newWQ b = if color == White then wq && not (kingMoved b) && not (qRookMoved b) else wq
        newBK b = if color == Black then bk && not (kingMoved b) && not (kRookMoved b) else bk
        newBQ b = if color == Black then bq && not (kingMoved b) && not (qRookMoved b) else bk
        newHM b = if (pawnMoved b) || getPieceCaptured board b then 0 else hm + 1
        newFM b = if color == Black then fm + 1 else fm


getEnPassantSquare :: Board -> Board -> (Int,Int)
getEnPassantSquare before after
  | abs (fst (getCoordinateMoved before after) - fst (getNewCoordinate before after)) < 2 = (9,9)
  | getPieceMoved before after /= (Pawn White) && getPieceMoved before after /= (Pawn Black) = (9,9)
  | getPieceMoved before after == (Pawn White) = (fst (getCoordinateMoved before after) + 1,snd $ getCoordinateMoved before after)
  | otherwise = (fst (getCoordinateMoved before after) - 1, snd $ getCoordinateMoved before after)

getPieceCaptured :: Board -> Board -> Bool
getPieceCaptured (PieceArr before) (PieceArr after) = foldr getCapturedRow False (zip before after)
    where
        getCapturedRow (rowBefore,rowAfter) recur = foldr getCapturedPiece False (zip rowBefore rowAfter) || recur
        getCapturedPiece (before,after) recur' = (getColor after /= getColor before && getColor after /= None && getColor before /= None) || recur'

getCoordinateMovedKing :: Board -> Board -> (Int,Int)
getCoordinateMovedKing (PieceArr before) (PieceArr after) = foldr getPieceRow (9,9) (zip3 before after [startCoord..])
    where
        getPieceRow (rowBefore,rowAfter,row) recur = if foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..]) == (9,9)
                                                     then recur
                                                     else foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..])
        getPiece (before,after,row,col) recur' = if after == Empty && (isKing before) then (row,col) else recur'

getCoordinateMovedPawn :: Board -> Board -> Color -> (Int,Int)
getCoordinateMovedPawn (PieceArr before) (PieceArr after) colr = foldr getPieceRow (9,9) (zip3 before after [startCoord..])
    where
        getPieceRow (rowBefore,rowAfter,row) recur = if foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..]) == (9,9)
                                                     then recur
                                                     else foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..])
        getPiece (before,after,row,col) recur' = if after == Empty && getColor before == colr  then (row,col) else recur'


getCoordinateMoved :: Board -> Board -> (Int,Int)
getCoordinateMoved (PieceArr before) (PieceArr after) = foldr getPieceRow (9,9) (zip3 before after [startCoord..])
    where
        getPieceRow (rowBefore,rowAfter,row) recur = if foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..]) == (9,9)
                                                     then recur
                                                     else foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..])
        getPiece (before,after,row,col) recur' = if after == Empty && before /= Empty then (row,col) else recur'

getNewCoordinatePawn :: Board -> Board -> (Int,Int)
getNewCoordinatePawn (PieceArr before) (PieceArr after) = foldr getPieceRow (9,9) (zip3 before after [startCoord..])
    where
        getPieceRow (rowBefore,rowAfter,row) recur = if foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..]) == (9,9)
                                                     then recur
                                                     else foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..])
        getPiece (before,after,row,col) recur' = if isPawn after && before /= after then (row,col) else recur'


getNewCoordinateKing :: Board -> Board -> (Int,Int)
getNewCoordinateKing (PieceArr before) (PieceArr after) = foldr getPieceRow (9,9) (zip3 before after [startCoord..])
    where
        getPieceRow (rowBefore,rowAfter,row) recur = if foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..]) == (9,9)
                                                     then recur
                                                     else foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..])
        getPiece (before,after,row,col) recur' = if isKing after && before /= after then (row,col) else recur'


getNewCoordinate :: Board -> Board -> (Int,Int)
getNewCoordinate (PieceArr before) (PieceArr after) = foldr getPieceRow (9,9) (zip3 before after [startCoord..])
    where
        getPieceRow (rowBefore,rowAfter,row) recur = if foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..]) == (9,9)
                                                     then recur
                                                     else foldr getPiece (9,9) (zip4 rowBefore rowAfter (repeat row) [startCoord..])
        getPiece (before,after,row,col) recur' = if after /= Empty && before /= after then (row,col) else recur'

getPieceMoved :: Board -> Board -> Piece
getPieceMoved (PieceArr before) (PieceArr after) = foldr getPieceRow Empty (zip before after)
    where
        getPieceRow (rowBefore,rowAfter) recur = if foldr getPiece Empty (zip rowBefore rowAfter) == Empty
                                                 then recur
                                                 else foldr getPiece Empty (zip rowBefore rowAfter)
        getPiece (before,after) recur' = if after == Empty && before /= Empty then before else recur'

-- Board -> Color -> EnPassant Square -> kingsideCastles possible -> queensideCastles possible -> List of legal moves
getLegalMovesFromBoard :: Board -> Color -> (Int,Int) -> Bool -> Bool -> [Board]
getLegalMovesFromBoard b c enP kCas qCas =
    [brd | brd <- getCandidateMovesForColor b c enP kCas qCas (getCandidateMovesForColorWithoutCastles b otherColor (-1,-1)), not (inCheck brd c)]
    where
        otherColor = if c == White then Black else White

kingOnBoard :: Board -> Color -> Bool
kingOnBoard (PieceArr pss) c = foldr (kingInRow c) False pss
    where
        kingInRow color row recur = foldr (kingOnSquare color) False row || recur
        kingOnSquare color' piece recur'
            | piece == King color' = True
            | otherwise = recur'

getCandidateMovesForColorWithoutCastles :: Board -> Color -> (Int,Int) -> [Board]
getCandidateMovesForColorWithoutCastles (PieceArr pss) color (enpx, enpy) = foldr parseRow [] (zip3 pss [startCoord..] (replicate 8 color))
    where
        parseRow (ps, row, color) recur = foldr parsePiece [] (zip3 ps (map (\i -> (row, i)) [startCoord..]) (replicate 8 color)) ++ recur
        parsePiece (p, (row, col), c) recur' = case c of

            White -> case p of
                        Pawn White -> getCandidatePawn (PieceArr pss) (row, col) (enpx, enpy) ++ recur'
                        Knight White -> getCandidateKnight (PieceArr pss) (row, col) ++ recur'
                        Bishop White -> getCandidateBishop (PieceArr pss) (row, col) ++ recur'
                        Rook White -> getCandidateRook (PieceArr pss) (row, col) ++ recur'
                        Queen White -> getCandidateQueen (PieceArr pss) (row, col) ++ recur'
                        King White -> getCandidateKingWithoutCastles (PieceArr pss) (row, col) ++ recur'
                        otherwise -> recur'
            Black -> case p of
                        Pawn Black -> getCandidatePawn (PieceArr pss) (row, col) (enpx, enpy) ++ recur'
                        Knight Black -> getCandidateKnight (PieceArr pss) (row, col) ++ recur'
                        Bishop Black -> getCandidateBishop (PieceArr pss) (row, col) ++ recur'
                        Rook Black -> getCandidateRook (PieceArr pss) (row, col) ++ recur'
                        Queen Black -> getCandidateQueen (PieceArr pss) (row, col) ++ recur'
                        King Black -> getCandidateKingWithoutCastles (PieceArr pss) (row, col) ++ recur'
                        otherwise -> recur'

-- pattern match for different pieces
-- Board -> Piece Coordinate -> EnPCoordinate -> QCastling -> KCastling -> List of possible boards

--To test: getCandidateMovesForColor startingPosition White (-1,-1) True True []
--         map boardToFEN (getCandidateMovesForColor ruyLopez White (-1,-1) True True [])
getCandidateMovesForColor :: Board -> Color -> (Int,Int) -> Bool -> Bool-> [Board] -> [Board]
getCandidateMovesForColor (PieceArr pss) color (enpx, enpy) kCas qCas enemyCandidates = foldr parseRow [] (zip3 pss [startCoord..] (replicate 8 color))
    where
        parseRow (ps, row, color) recur = foldr parsePiece [] (zip3 ps (map (\i -> (row, i)) [startCoord..]) (replicate 8 color)) ++ recur
        parsePiece (p, (row, col), c) recur' = case c of

            White -> case p of
                        Pawn White -> getCandidatePawn (PieceArr pss) (row, col) (enpx, enpy) ++ recur'
                        Knight White -> getCandidateKnight (PieceArr pss) (row, col) ++ recur'
                        Bishop White -> getCandidateBishop (PieceArr pss) (row, col) ++ recur'
                        Rook White -> getCandidateRook (PieceArr pss) (row, col) ++ recur'
                        Queen White -> getCandidateQueen (PieceArr pss) (row, col) ++ recur'
                        King White -> getCandidateKing (PieceArr pss) (row, col) kCas qCas enemyCandidates ++ recur'
                        otherwise -> recur'
            Black -> case p of
                        Pawn Black -> getCandidatePawn (PieceArr pss) (row, col) (enpx, enpy) ++ recur'
                        Knight Black -> getCandidateKnight (PieceArr pss) (row, col) ++ recur'
                        Bishop Black -> getCandidateBishop (PieceArr pss) (row, col) ++ recur'
                        Rook Black -> getCandidateRook (PieceArr pss) (row, col) ++ recur'
                        Queen Black -> getCandidateQueen (PieceArr pss) (row, col) ++ recur'
                        King Black -> getCandidateKing (PieceArr pss) (row, col) kCas qCas enemyCandidates ++ recur'
                        otherwise -> recur'

-- In the future: Add other promotions
-- Board -> Piece Coordinate -> En Passant Coordinate -> List of Boards
getCandidatePawn :: Board -> (Int,Int) -> (Int,Int) -> [Board]
getCandidatePawn (PieceArr pss) (x,y) (enPx,enPy) = boardWithEnPassant ++ boardWithCandidates ++ boardWithQueenPromotion
                                                    -- ++ boardWithKnightPromotion ++ boardWithBishopPromotion ++ boardWithRookPromotion
                                                    
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
            | onStartingRank = checkDiags 1 ++ checkDiags (-1) ++ checkForward 1 ++ if length (checkForward 1) > 0 then checkForward 2
             else []
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
            | colorKing == White = kCas && (checkSquaresForEmpty2 (1,6) (1,7)) && not (checkSquaresForThreat (1,5) (1,6) (1,7) Black)
            | colorKing == Black = kCas && (checkSquaresForEmpty2 (8,6) (8,7)) && not (checkSquaresForThreat (8,5) (8,6) (8,7) White)

        castleQueenside
            | colorKing == White = qCas && (checkSquaresForEmpty3 (1,4) (1,3) (1,2)) && not (checkSquaresForThreat (1,3) (1,4) (1,5) Black)
            | colorKing == Black = qCas && (checkSquaresForEmpty3 (8,4) (8,3) (8,2)) && not (checkSquaresForThreat (8,3) (8,4) (8,5) White)

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



-- Helper Functions

validSquare :: (Int,Int) -> Bool
validSquare (x,y) = x >= startCoord && x <= endCoord && y >= startCoord && y <= endCoord

emptySquare :: Board -> (Int,Int) -> Bool
emptySquare (PieceArr pss) (x,y) = (pss !! (x - startCoord)) !! (y - startCoord) == Empty

colorSquare :: Board -> (Int,Int) -> Color
colorSquare (PieceArr pss) (x,y) = getColor (pss !! (x - startCoord) !! (y - startCoord))

getColor :: Piece -> Color
getColor Empty = None
getColor (Pawn c) = c
getColor (Knight c) = c
getColor (Bishop c) = c
getColor (Queen c) = c
getColor (King c) = c
getColor (Rook c) = c

pieceArrFromBoard :: Board -> [[Piece]]
pieceArrFromBoard (PieceArr pss) = pss

inCheck :: Board -> Color -> Bool
inCheck b c = not (all (`kingOnBoard` c) possibleBoards)
     where
        otherColor = if c == White then Black else White
        -- Generating candidate boards
        possibleBoards = getCandidateMovesForColorWithoutCastles b otherColor (-1,-1)

isCheckMateFromParameters :: Board -> Color -> (Int,Int) -> Bool
isCheckMateFromParameters brd clr enp = null (getLegalMovesFromBoard brd clr enp True True) && inCheck brd clr

isStaleMateFromParameters :: Board -> Color -> (Int,Int) -> Bool
isStaleMateFromParameters brd clr enp = null (getLegalMovesFromBoard brd clr enp True True) && not (inCheck brd clr)

repetition :: Position -> Position -> Bool
repetition (Position b1 _ wk1 wq1 bk1 bq1 _ _ _) (Position b2 _ wk2 wq2 bk2 bq2 _ _ _)
    = b1 == b2 && wk1 == wk2 && bk1 == bk2 && bq1 == bq2

-- GAME OVER STUFF

isFiftyMove :: String -> Bool
isFiftyMove fen = isFifty (positionFromFEN fen)
    where
        isFifty (Position _ _ _ _ _ _ _ halfMove _) = halfMove >= 50

isCheckMate :: String -> Bool
isCheckMate fen = isCheck (positionFromFEN fen)
    where
        isCheck (Position board color _ _ _ _ enP _ _) = isCheckMateFromParameters board color enP

isStaleMate :: String -> Bool
isStaleMate fen = isStale (positionFromFEN fen)
    where
        isStale (Position board color _ _ _ _ enP _ _) = isStaleMateFromParameters board color enP


isRepetition :: [String] -> Bool
isRepetition = undefined
-- Possibly define in the future


