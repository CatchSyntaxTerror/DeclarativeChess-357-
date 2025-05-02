module ChessLogic.ChessFunctions where

--import ChessLogic.Types
--import ChessLogic.ChessConstants

--boardFromFEN :: String -> Board
--boardFromFEN = undefined


rowsFromFEN :: String -> [String]
rowsFromFEN fen = getRow fen 0
    where
        getRow ('/':cs) n = getRow cs n
        getRow fen 7 = [fen]
        getRow fen n = takeWhile (/= '/') fen : getRow (dropWhile (/= '/') fen) (n + 1)



--parseFENrow :: String -> Int -> [Piece]
--parseFENrow strs r = undefined --foldr go [] zip strs [startCoord..]
--    where
--        go ('p',c) recur = Pawn   Black (r,c) : recur
--        go ('n',c) recur = Knight Black (r,c) : recur
--        go ('b',c) recur = Bishop Black (r,c) : recur
--        go ('r',c) recur = Rook   Black (r,c) : recur
--        go ('q',c) recur = Queen  Black (r,c) : recur
--        go ('k',c) recur = King   Black (r,c) : recur
--        go ('P',c) recur = Pawn   White (r,c) : recur
--        go ('N',c) recur = Knight White (r,c) : recur
--        go ('B',c) recur = Bishop White (r,c) : recur
--        go ('R',c) recur = Rook   White (r,c) : recur
--        go ('Q',c) recur = Queen  White (r,c) : recur
--        go ('K',c) recur = King   White (r,c) : recur
        

--boardToFEN :: Board -> String
--boardToFEN = undefined