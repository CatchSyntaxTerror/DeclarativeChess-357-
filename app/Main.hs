module Main where

import Brillo
import ChessLogic

main :: IO ()
main = do
  putStrLn "Game Loading..."

  MyLib.someFunc
  display (InWindow "Declarative Chess" (600, 600) (100, 100))



squareSize :: float
squareSize = 50

chessBoard :: Picture
chessBoard = Pictures
  [translate (x * squareSize - 175) (y * squareSize - 175) 
             (Color (tileColor x y) (rectangleSolid squareSize squareSize)) | x <- [0..7], y <- [0..7] ]
  where
    tileColor x y = if even (x + y) then cream else greenPastel
    cream = makeColor 1.0 0.98 0.88 1.0
    greenPastel = makeColor 0.4 0.6 0.5 1.0