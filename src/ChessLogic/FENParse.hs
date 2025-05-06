module ChessLogic.FENParse where

import ChessLogic.Types

import Data.Char (digitToInt)
import Data.Char (intToDigit)

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