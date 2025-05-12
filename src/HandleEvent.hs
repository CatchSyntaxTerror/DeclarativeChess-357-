module HandleEvent where

import ChessLogic.ChessFunctions as Chess
import ChessLogic.ChessAI as ComputerPlayer
import ChessLogic.Types
import ChessLogic.FENParse
import ChessLogic.ChessConstants

import Brillo.Interface.IO.Interact

data ClickState = ClickState {
    fen :: String,
    selectedSquare :: (Int,Int),
    targetSquare :: (Int,Int)
}

sqs = 100 :: Float
bs = 800 :: Float

ai = True :: Bool -- False = AI disabled & True = AI enabled

ailevel = 3 :: Int -- specify ai level (1,2,3,4) defaulting to 3

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
    | Chess.isCheckMate fen && getFENColor fen == Black = 
        (ClickState whiteWins (9,9) (9,9))
    | Chess.isCheckMate fen && getFENColor fen == White =
        (ClickState blackWins (9,9) (9,9))
    | Chess.isStaleMate fen = (ClickState draw (9,9) (9,9))
    | tar /= (9,9) =
        let newFen = Chess.newPositionFromCoordinates fen sel tar
        in ClickState newFen (if getFENColor newFen == Black then (9,9) else tar) (9,9)
    | ai && isBlacksTurn fen && sel == (9,9) && tar == (9,9) =
        let (from, to) = aiMove (depth ailevel) fen -- depth d (specify d)
            finalFen   = Chess.newPositionFromCoordinates fen from to
        in ClickState finalFen (9,9) (9,9)
    | otherwise = before

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