module ChessLogic.ChessConstants where

import ChessLogic.FENParse


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

startCoord :: Int
startCoord = 1

endCoord :: Int
endCoord = 8

