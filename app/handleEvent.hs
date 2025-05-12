-- module HandleEvent where

-- import ChessLogic.ChessFunctions as Chess

-- import Brillo.Interface.IO.Interact

-- data ClickState = ClickState {
--     fen :: String,
--     selectedSquare :: (Int,Int),
--     targetSquare :: (Int,Int)
-- }

-- sqs = 100 :: Float
-- bs = 800 :: Float

-- convertToNormal :: (Float,Float) -> (Int,Int)
-- convertToNormal (x,y) = ((floor ((x - bs / 2)/100) :: Int) + 1, (floor (((-y) + bs / 2)/100) :: Int) + 1)

-- test :: Event -> (Int,Int)
-- test (EventKey (MouseButton leftButton) Down _ (x,y)) = undefined

-- handleClickEvent :: Event -> ClickState -> ClickState
-- handleClickEvent (EventKey (MouseButton leftButton) Down _ (x,y)) clkS = handleClickCoordinate clkS convertToNormal (x,y)

-- handleClickCoordinate :: ClickState -> (Int,Int) -> ClickState
-- handleClickCoordinate before@(ClickState fen sel tar) coord
--     | sel == (9,9) = (ClickState fen coord tar)
--     | tar == (9,9) = (ClickState fen sel coord)
--     | otherwise = before

-- updateClickState :: ClickState -> ClickState
-- updateClickState before@(ClickState fen sel tar)
--     | tar == (9,9) = before
--     | otherwise = ClickState (Chess.newPositionFromCoordinates fen sel tar) (9,9) (9,9)

-- -- newEventHandle :: Event -> BoardState -> BoardState
-- -- newEventHandle (EventKey (MouseButton LeftButton) Up _ (x,y)) board = undefined
-- -- newEventHandle _ _ = undefined

-- -- takes a mouse click event and returns the coordinate on the chess board
-- clickToCoordinate :: (Int,Int) -> (Int,Int)
-- clickToCoordinate = undefined