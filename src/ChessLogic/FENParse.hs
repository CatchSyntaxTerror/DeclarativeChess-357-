module ChessLogic.FENParse where

import ChessLogic.Types

import Data.Char (digitToInt)
import Data.Char (intToDigit)

startCoordinate = 1 :: Int

positionToFEN :: Position -> String
positionToFEN (Position b c wK wQ bK bQ enP hm fm) = 
    boardToFEN b ++ " " ++ [colorToChar c] ++ " " ++ castlesString wK wQ bK bQ ++ " " ++ coordToNotation enP ++ " " ++ show hm ++ " " ++  show fm
        where
            colorToChar c = if c == White then 'w' else 'b'
            castlesString False False False False = "-"
            castlesString wk wq bk bq = wkString wk ++ wqString wq ++ bkString bk ++ bqString bq
            coordToNotation (9,9) = "-"
            coordToNotation (row,col) = intToFile col : intToDigit row : []

            wkString w = if w then "K" else []
            wqString w = if w then "Q" else []
            bkString b = if b then "k" else []
            bqString b = if b then "q" else [] 

positionFromFEN :: String -> Position
positionFromFEN fen = Position board color wKingside wQueenside bKingside bQueenside enPassant halfMove fullMove
    where
        board = boardFromFEN (takeWhile (/= ' ') fen)
        color = if head (getField 1 fen) == 'w' then White else Black
        wKingside = elem 'K' (getField 2 fen)
        wQueenside = elem 'Q' (getField 2 fen)
        bKingside = elem 'k' (getField 2 fen)
        bQueenside = elem 'q' (getField 2 fen)
        enPassant = notationToCoordinate (getField 3 fen)
        halfMove = read (getField 4 fen) :: Int
        fullMove = read (getField 5 fen) :: Int

getField :: Int -> String -> String
getField 0 fen = if elem ' ' fen then takeWhile (/= ' ') fen else fen
getField n fen = getField (n - 1) (tail (dropWhile (/= ' ') fen))

notationToCoordinate :: String -> (Int,Int)
notationToCoordinate [col,row] = (fileToInt col, digitToInt row)
notationToCoordinate _ = (9,9)

intToFile :: Int -> Char
intToFile 1 = 'a'
intToFile 2 = 'b'
intToFile 3 = 'c'
intToFile 4 = 'd'
intToFile 5 = 'e'
intToFile 6 = 'f'
intToFile 7 = 'g'
intToFile 8 = 'h'
intToFile _ = 'x'

fileToInt :: Char -> Int
fileToInt 'a' = startCoordinate
fileToInt 'b' = startCoordinate + 1
fileToInt 'c' = startCoordinate + 2
fileToInt 'd' = startCoordinate + 3
fileToInt 'e' = startCoordinate + 4
fileToInt 'f' = startCoordinate + 5
fileToInt 'g' = startCoordinate + 6
fileToInt 'h' = startCoordinate + 7
fileToInt _ = 9

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