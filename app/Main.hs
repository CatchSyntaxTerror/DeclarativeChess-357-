module Main where

import Brillo
import Brillo.Data.Color as BColor

import ChessLogic.Types as Chess

squareSize :: Float
squareSize = 100.0

cream :: BColor.Color
cream = BColor.makeColor 1.0 0.9 0.7 1.0

greenPastel :: BColor.Color
greenPastel = BColor.makeColor 0.4 0.6 0.5 1.0

tileColor :: Int -> Int -> BColor.Color
tileColor x y = if even (x + y) then cream else greenPastel


tile :: Int -> Int -> Picture
tile x y = color (tileColor x y) (rectangleSolid squareSize squareSize)

pieceToPicture :: Piece -> Picture
pieceToPicture p = scale 0.15 0.15 $ translate (-200) (-200) $
    text (showPiece p)

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
  putStrLn "Finished"
