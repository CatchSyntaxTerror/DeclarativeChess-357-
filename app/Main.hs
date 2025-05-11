module Main where

import Brillo
import Brillo.Data.Color as BColor

import ChessLogic.Types as Chess
import ChessLogic.ChessConstants

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
  [ translate 0 (-5) $ scale 0.3 0.3 $ circleSolid 20   -- Head
  , translate 0 (-10) $ rectangleSolid 5 5             -- Mid1
  , translate 0 (-16) $ scale 2 0.5 $ rectangleSolid 7 7 -- Mid2
  , translate 0 (-20) $ scale 2 0.5 $ rectangleSolid 10 10 -- Base
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
  [ translate 0 (-4) $ rectangleSolid 10 10 -- 
  , translate (-4) (-7) $ rectangleSolid 7 7 -- 
  , translate (-7) (-11) $ rectangleSolid 5 5 -- 
  , translate 0 (-15) $ rectangleSolid 5 5 -- Neck
  , translate 0 (-20) $ rectangleSolid 10 7 -- Mid1
  , translate 0 (-25) $ rectangleSolid 20 7 -- Mid2
  , translate 0 (-30) $ rectangleSolid 30 7 -- Base
  ]

drawBishop :: BColor.Color -> Picture
drawBishop c = color c $ pictures
  [ translate 0 0 $ rectangleSolid 3 3 -- Top separator
  , translate 0 (-5) $ rectangleSolid 7 5 -- Top separator
  , translate 0 (-9) $ rectangleSolid 13 2 -- Top separator
  , translate 0 (-12) $ rectangleSolid 5 5 -- Mid1
  , translate 0 (-20) $ rectangleSolid 6 6 -- Mid2
  , translate 0 (-25) $ rectangleSolid 10 5 -- Mid3
  , translate 0 (-30) $ rectangleSolid 20 7 -- Base
  ]

drawQueen :: BColor.Color -> Picture
drawQueen c = color c $ pictures
  [ translate 0 0 $ scale 0.6 0.6 $ circleSolid 10
  , translate 0 (-10) $ circleSolid 6
  , translate 0 (-20) $ rectangleSolid 12 18
  , translate 0 (-35) $ scale 2 0.5 $ rectangleSolid 10 10
  ]

drawKing :: BColor.Color -> Picture
drawKing c = color c $ pictures
  [ translate 0 0 $ scale 0.6 0.6 $ circleSolid 10
  , translate 0 (-10) $ rectangleSolid 4 10
  , translate 0 (-20) $ rectangleSolid 12 18
  , translate 0 (-20) $ scale 2 0.5 $ rectangleSolid 10 10
  ]

pieceToPicture :: Piece -> Picture
pieceToPicture p = translate (0) (20) $ scale 1.5 1.5 $ pieceSymbol p

squareSize :: Float
squareSize = 100.0

cream :: BColor.Color
cream = BColor.makeColor 1.0 0.9 0.7 1.0

greenPastel :: BColor.Color
greenPastel = BColor.makeColor 0.4 0.6 0.5 1.0

tileColor :: Int -> Int -> BColor.Color
tileColor x y = if even (x + y) then makeColor 0.50 0.50 0.50 1.0 else makeColor 0.3 0.3 0.3 1.0

--nf3 = head (tail (getCandidateKnight startingPosition (1,7)))

tile :: Int -> Int -> Picture
tile x y = color (tileColor x y) (rectangleSolid squareSize squareSize)

boardToPicture :: Float -> Board -> Picture
boardToPicture ws (PieceArr grid) = pictures
    [ translate' ws cx cy (pictures [tile x y, pieceToPicture p])
    | (y, row) <- zip [0..] grid
    , (x, p)   <- zip [0..] row
    , let cx = fromIntegral x * squareSize + squareSize / 2
          cy = fromIntegral y * squareSize + squareSize / 2
    ]

--translate the coordinates to the center of the window
translate' :: Float -> Float -> Float -> Picture -> Picture
translate' boardSize x y = translate (x - boardSize / 2) ((-y) + boardSize / 2)


main :: IO ()
main = do
  let board = startingPosition
  let ws = 8 * squareSize

  putStrLn "Loading..."
  display (InWindow "Declarative Chess" (round ws, round ws) (100, 100)) white (boardToPicture ws board)
--   putStrLn "Finished"
--     display (InWindow "Declarative Chess1" (600, 600) (10, 10)) red (Color white (Circle 80))
