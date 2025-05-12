module ChessLogic.ChessConstants where

import ChessLogic.FENParse


-- Starting position constructed as piece array for test
startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" :: String
startingPosition = boardFromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

-- Some Test Positions
oldBenoni = boardFromFEN "rnbqkbnr/pp1ppppp/8/2p5/3P4/8/PPP1PPPP/RNBQKBNR"
testPawnCapture = boardFromFEN "rnbqkbnr/pp1ppppp/8/8/3P4/2p5/PPP1PPPP/RNBQKBNR"
knightFThree = boardFromFEN "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R"
queensPawn = boardFromFEN "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR"
queensPawnGame = boardFromFEN "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR"
staleMate = boardFromFEN "kb6/qb5r/8/8/8/7N/6R1/r4B1K"
smotheredMate = boardFromFEN "r1b1kbnr/pppp1Npp/8/8/4q3/5n2/PPPPBP1P/RNBQKR2"
testKingOnBoard = boardFromFEN "rnbqbbnr/ppp2ppp/3p4/1B2p3/4P3/5N2/PPPP1PPP/RNBQB2R"
testCheck = boardFromFEN "rnbqkbnr/ppp2ppp/3p4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R"
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

-- Some test FENs
draw = "2p2P2/1pPpPpP1/2pPpP2/3pP3/8/3Kk3/8/8 w - - 0 1"
blackWins = "8/2r2r2/2pkqp2/3pp3/r1r2r1r/ppp2ppp/ppp2ppp/pQp2pKp w - - 0 1"
whiteWins = "8/2R2R2/2PKQP2/3PP3/R1R2R1R/PPP2PPP/PPP2PPP/PqP2PkP w - - 0 1"
ruyFENwtm = "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 3 3"
ruyFEN = "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
italianFEN = "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
checkFEN = "rnbqkbnr/ppp2ppp/3p4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 1 3"
smotheredMateFEN = "r1b1kbnr/pppp1Npp/8/8/4q3/5n2/PPPPBP1P/RNBQKR2 w Qkq - 2 8"

startCoord :: Int
startCoord = 1

endCoord :: Int
endCoord = 8

