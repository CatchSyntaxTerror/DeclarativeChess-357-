module Main where

import Brillo
import Brillo.Data.Color as BColor


import ChessLogic.Types as Chess
import ChessLogic.ChessConstants
import ChessLogic.FENParse as FENParse
import ChessLogic.ChessFunctions as ChessFunctions

--import HandleEvent
import TestHandleEvent

pieceSymbol :: Piece -> Picture
pieceSymbol (Pawn White)   = drawPawn white
pieceSymbol (Pawn Black)   = drawPawn black
pieceSymbol (Rook White)   = drawRook white
pieceSymbol (Rook Black)   = drawRook black
pieceSymbol (Knight White) = drawKnight white
pieceSymbol (Knight Black) = drawKnight black
pieceSymbol (Bishop White) = drawBishop white
pieceSymbol (Bishop Black) = drawBishop black
pieceSymbol (Queen White)  = drawQueen white
pieceSymbol (Queen Black)  = drawQueen black
pieceSymbol (King White)   = drawKing white
pieceSymbol (King Black)   = drawKing black
pieceSymbol _ = blank

drawPawn :: BColor.Color -> Picture
drawPawn c = color c $ pictures
  [ translate 0 (-2) $ rectangleSolid 7 5 -- Head1
  , translate 0 (-4) $ rectangleSolid 12 5 -- Head2
  , translate 0 (-6) $ rectangleSolid 7 5 -- Head3
  , translate 0 (-12) $ rectangleSolid 10 4 -- Mid2
  , translate 0 (-16) $ rectangleSolid 20 4 -- Mid3
  , translate 0 (-20) $ rectangleSolid 25 4 -- Base
  ]

drawRook :: BColor.Color -> Picture
drawRook c = color c $ pictures
  [ translate (10) (-5) $ rectangleSolid 5 5 -- Tip1
  , translate 0 (-5) $ rectangleSolid 5 5 -- Tip2
  , translate (-10) (-5) $ rectangleSolid 5 5 -- Tip3
  , translate 0 (-10) $ rectangleSolid 25 5 -- Top Base
  , translate 0 (-15) $ rectangleSolid 15 10 -- Mid1
  , translate 0 (-25) $ rectangleSolid 20 7 -- Mid2
  , translate 0 (-30) $ rectangleSolid 30 7 -- Base
  ]

drawKnight :: BColor.Color -> Picture
drawKnight c = color c $ pictures
  [ translate 0 (-4) $ rectangleSolid 10 10 -- Head
  , translate (-6) (-8) $ rectangleSolid 9 9 -- Snout1
  , translate (-10) (-12) $ rectangleSolid 8 8 -- Snout2
  , translate (-2) (1.3) $ rectangleSolid 12 4 -- Snout3
  , translate (-7) (-1.7) $ rectangleSolid 9 4 -- Snout4
  , translate (-13) (-5.7) $ rectangleSolid 4.5 4.5 -- Snout5
  , translate (-15) (-10.5) $ rectangleSolid 5 5 -- Snout6
  , translate (4) (-0.5) $ rectangleSolid 4 4 -- Neck1
  , translate (4) (-5) $ rectangleSolid 8 8 -- Neck2
  , translate 5 (-15) $ rectangleSolid 8 7 -- Mid1
  , translate (3) (-20) $ rectangleSolid 10 7 -- Mid2
  , translate 0 (-25) $ rectangleSolid 20 7 -- Mid3
  , translate 0 (-30) $ rectangleSolid 30 7 -- Base
  ]

drawBishop :: BColor.Color -> Picture
drawBishop c = color c $ pictures
  [ translate 0 0 $ rotate 45 $ rectangleSolid 5 5 -- Tip
  , translate 0 (-5) $ rectangleSolid 7 3 -- Top
  , translate 0 (-9) $ rectangleSolid 13 2 -- Top separator
  , translate 0 (-12) $ rectangleSolid 5 5 -- Mid1
  , translate 0 (-20) $ rectangleSolid 6 6 -- Mid2
  , translate 0 (-25) $ rectangleSolid 10 5 -- Mid3
  , translate 0 (-30) $ rectangleSolid 20 7 -- Base
  ]

drawQueen :: BColor.Color -> Picture
drawQueen c = color c $ pictures
  [ translate 0 (2) $ rectangleSolid 5 5
  , translate 0 (-2) $ rectangleSolid 14 5
  , translate 0 (-4) $ rectangleSolid 12 10
  , translate 0 (-15) $ rectangleSolid 13 2
  , translate 0 (-20) $ rectangleSolid 6 18
  , translate 0 (-21) $ rectangleSolid 8 11
  , translate 0 (-30) $ rectangleSolid 15 5
  , translate 0 (-35) $ rectangleSolid 20 7
  ]

drawKing :: BColor.Color -> Picture
drawKing c = color c $ pictures
  [ translate 0 (5) $ rectangleSolid 3 9
  , translate 0 (6) $ rectangleSolid 8 3
  , translate 0 (-3) $ rectangleSolid 12 10
  , translate 0 (-5) $ rectangleSolid 10 10
  , translate 0 (-15) $ rectangleSolid 13 2
  , translate 0 (-20) $ rectangleSolid 6 18
  , translate 0 (-21) $ rectangleSolid 8 11
  , translate 0 (-30) $ rectangleSolid 15 5
  , translate 0 (-35) $ rectangleSolid 20 7
  ]

drawLoadingSprites :: BColor.Color -> Picture
drawLoadingSprites c = color c $ pictures
  [ translate 0 0 $ rectangleSolid 3 9
  , translate (20) 0 $ rectangleSolid 8 3
  , translate (10) 0 $ rectangleSolid 12 10
  ]

pieceToPicture :: Piece -> Picture
pieceToPicture p = translate (0) (20) $ scale 1.5 1.5 $ pieceSymbol p

squareSize :: Float
squareSize = 100.0

tileColor :: Int -> Int -> BColor.Color
tileColor x y = if even (x + y) then makeColor 0.50 0.50 0.50 1.0 else makeColor 0.3 0.3 0.3 1.0

highlightColor :: Int -> Int -> BColor.Color
highlightColor x y = if even (x + y) then makeColor 0 0.6 0.6 1.0 else makeColor 0 0.6 0.6 1.0

tile :: Int -> Int -> Picture
tile x y = color (tileColor x y) (rectangleSolid squareSize squareSize)

tileLit :: Int -> Int -> Picture
tileLit x y = color (highlightColor x y) (rectangleSolid squareSize squareSize)

tileHighlight :: Int -> Int -> Picture
tileHighlight x y = color (highlightColor x y) (rectangleSolid squareSize squareSize)

boardToPicture :: Float -> Board -> Picture
boardToPicture ws (PieceArr grid) = pictures
    [ translate' ws cx cy (pictures [tile x y, pieceToPicture p])
    | (y, row) <- zip [0..] flippedGrid
    , (x, p)   <- zip [0..] row
    , let cx = fromIntegral x * squareSize + squareSize / 2
          cy = fromIntegral y * squareSize + squareSize / 2
    ]
    where flippedGrid = reverse grid

boardSquareToPicture :: Float -> Position -> (Int,Int) -> Picture
boardSquareToPicture ws pos@(Position (PieceArr grid) _ _ _ _ _ _ _ _) (row,col) = pictures
    [ translate' ws cx cy (pictures [if elem (8 - y,x + 1) highlightedSquares then tileLit x y else tile x y, pieceToPicture p])
    | (y, r) <- zip [0..] flippedGrid
    , (x, p) <- zip [0..] r
    , let cx = fromIntegral x * squareSize + squareSize / 2
          cy = fromIntegral y * squareSize + squareSize / 2
    ]
    where 
        flippedGrid = reverse grid
        highlightedSquares = (row,col) : ChessFunctions.getLegalSquaresForCoordinate (FENParse.positionToFEN pos) (row,col)

--translate the coordinates to the center of the window
translate' :: Float -> Float -> Float -> Picture -> Picture
translate' boardSize x y = translate (x - boardSize / 2) ((-y) + boardSize / 2)


initClickState = ClickState startFEN (9,9) (9,9)

clickStateToPicture :: Float -> ClickState -> Picture
clickStateToPicture ws (ClickState fen sel _) = boardSquareToPicture ws (FENParse.positionFromFEN fen) sel

main :: IO ()
main = do
  let board = startingPosition
  let startPos = FENParse.positionFromFEN ruyFENwtm
  let checkPos = FENParse.positionFromFEN checkFEN
  let ws = 8 * squareSize

  --putStrLn "Loading..."
  --display (InWindow "Declarative Chess" (round ws, round ws) (100, 100)) white (boardToPicture ws board)

--   display (InWindow "Declarative Chess" (round ws, round ws) (100, 100)) white (boardSquareToPicture ws checkPos (8,2))
  --play (InWindow "Declarative Chess" (round ws, round ws) (100,100)) white (boardToPicture ws board)

  play
    (InWindow "Declarative Chess" (round ws, round ws) (100,100))
    white 
    60 -- This is the FPS of the game
    initClickState
    (\clk -> clickStateToPicture ws clk)
    --(boardToPicture ws board) -- This line Determines the starting state of the game
    --function for converting board to picture(?)
    TestHandleEvent.handleClickEvent
    TestHandleEvent.updateClickState
