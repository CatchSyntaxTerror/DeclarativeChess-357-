module TestHandleEvent where

import ChessLogic.ChessFunctions as Chess
import ChessLogic.ChessAI as ComputerPlayer

import Brillo.Interface.IO.Interact

data ClickState = ClickState {
    fen :: String,
    selectedSquare :: (Int,Int),
    targetSquare :: (Int,Int)
}

sqs = 100 :: Float
bs = 800 :: Float

convertToNormal :: (Float,Float) -> (Int,Int)
convertToNormal (x,y) = ((ceiling (y/100 + 4) :: Int), ceiling (x/100 + 4) :: Int)

test :: Event -> (Int,Int)
test (EventKey (MouseButton leftButton) Down _ (x,y)) = undefined

handleClickEvent :: Event -> ClickState -> ClickState
handleClickEvent (EventKey (MouseButton leftButton) Down _ (x,y)) clkS = handleClickCoordinate clkS (convertToNormal (x,y))
handleClickEvent _ clkS = clkS

handleClickCoordinate :: ClickState -> (Int,Int) -> ClickState
handleClickCoordinate before@(ClickState fen sel tar) coord
    | sel == (9,9) = (ClickState fen coord tar)
    | tar == (9,9) = (ClickState fen sel coord)
    | otherwise = before

updateClickState :: Float -> ClickState -> ClickState
updateClickState _ before@(ClickState fen sel tar)
    | tar == (9,9) = before
    | otherwise =
        let newFen = Chess.newPositionFromCoordinates fen sel tar
        in if isBlacksTurn newFen
            then
                let (from, to) = aiMove newFen  -- ← your AI's chosen move
                    finalFen = Chess.newPositionFromCoordinates newFen from to
                in ClickState finalFen (9,9) (9,9)
            else
                ClickState newFen tar (9,9)

isBlacksTurn :: String -> Bool
isBlacksTurn fen = case words fen of
    (_:color:_) -> color == "b"
    _ -> False

-- newEventHandle :: Event -> BoardState -> BoardState
-- newEventHandle (EventKey (MouseButton LeftButton) Up _ (x,y)) board = undefined
-- newEventHandle _ _ = undefined

-- takes a mouse click event and returns the coordinate on the chess board
clickToCoordinate :: (Int,Int) -> (Int,Int)
clickToCoordinate = undefined