module Main where

import Brillo
import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
