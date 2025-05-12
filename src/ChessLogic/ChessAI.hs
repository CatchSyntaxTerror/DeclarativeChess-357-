{-# LANGUAGE BangPatterns #-}
module ChessLogic.ChessAI where

import ChessLogic.ChessFunctions as ChessFunctions
import ChessLogic.FENParse (positionFromFEN)
import Data.Function (on)
import Data.List (maximumBy)
  
-- Chess piece constructor
data Piece = 
       Pawn   { pVal :: Int }
     | Knight { pVal :: Int }
     | Bishop { pVal :: Int }
     | Rook   { pVal :: Int }
     | Queen  { pVal :: Int }
     | King   { pVal :: Int }
     deriving (Eq, Show)

-- Chess piece material values (Constructor value initialization)
pawn, knight, bishop, rook, queen, king :: Piece
pawn   = Pawn   1
knight = Knight 3
bishop = Bishop 3
rook   = Rook   5
queen  = Queen  9
king   = King   500

-- Convert FEN character to material value
charValue :: Char -> Int
charValue c
  | c == 'P'  = -(pVal pawn)
  | c == 'p'  = pVal pawn
  | c == 'N'  = -(pVal knight)
  | c == 'n'  = pVal knight
  | c == 'B'  = -(pVal bishop)
  | c == 'b'  = pVal bishop
  | c == 'R'  = -(pVal rook)
  | c == 'r'  = pVal rook
  | c == 'Q'  = -(pVal queen)
  | c == 'q'  = pVal queen
  | c == 'K'  = -(pVal king)
  | c == 'k'  = pVal king
  | c `elem` ['1'..'8'] = 0 -- Empty squares
  | c == '/' = 0           -- Ignore rank separators
  | otherwise = 0          -- Safety fallback

inf :: Int
inf = 10 ^ 9

-- Chess game tree depth value
depth :: Int -> Int
depth x = x

-- Game Tree Data Type (FEN, Material Score) [Child Nodes]
data GameTree = Node (String, Int) [GameTree]
  deriving (Show, Eq)

-- Create chess game tree with 1st node being the material score-maximizing node
buildTree :: Int -> String -> GameTree
buildTree h fen = buildTree' h True fen

buildTree' :: Int -> Bool -> String -> GameTree
buildTree' 0 maximizing fen =
  Node (fen, generateMaterialScore maximizing fen []) []
buildTree' h maximizing fen =
  let rawChildren    = generateChildren (h - 1) (not maximizing) (fenTOfens fen)
      prunedChildren = pruneChildren maximizing rawChildren
      nodeScore      = generateMaterialScore maximizing fen prunedChildren
  in Node (fen, nodeScore) prunedChildren

-- Generate Child Nodes for a Parent Node aka generate all possible moves for a given FEN
generateChildren :: Int -> Bool -> [String] -> [GameTree]
generateChildren h maximizing fens = map (buildTree' h maximizing) fens

-- Take a FEN and returns an array of FENs aka an array of all possible moves
fenTOfens :: String -> [String]
fenTOfens fen = ChessFunctions.getLegalMoves fen

-- Evaluate an FEN and generate it's material score
generateMaterialScore :: Bool -> String -> [GameTree] -> Int
generateMaterialScore maximizingPlayer fen children =
  if null children
    then evalFEN fen
    else minimax maximizingPlayer children

--Minimax of all child nodes based on the player turn
minimax :: Bool -> [GameTree] -> Int
minimax maximizingPlayer children =
  let scores = map (\(Node (_, s) _) -> s) children
  in if maximizingPlayer
       then maximum scores
       else minimum scores

-- Quick static evaluation of an FEN without calling minimax
evalFEN :: String -> Int
evalFEN fen = 
  let score = sum (map charValue (takeWhile (/= ' ') fen))
  in if whiteToMove fen then -score else score

whiteToMove :: String -> Bool
whiteToMove fen = case words fen of
    (_:color:_) -> color == "w"
    _ -> False

-- Inline Alpha Beta Pruning
alphaBeta :: Int -> Bool -> String -> Int -> Int -> Int
alphaBeta 0 _ fen _ _ = evalFEN fen
alphaBeta d maximizing fen !alpha !beta =
  search alpha (ChessFunctions.getLegalMoves fen)
  where
    search !a []     = a
    search !a (m:ms) =
      let score = - alphaBeta (d - 1) (not maximizing) m (-beta) (-a)
          a'    = max a score
      in if a' >= beta
           then a'
           else search a' ms

-- Keep only those child nodes whose score equals the current best
-- (max for the maximizing player, min for the minimizing player)
pruneChildren :: Bool -> [GameTree] -> [GameTree]
pruneChildren _ [] = []
pruneChildren maximizing xs =
  let scores = map (\(Node (_, s) _) -> s) xs
      best   = if maximizing then maximum scores else minimum scores
  in filter (\(Node (_, s) _) ->
               if maximizing
                 then s >= best   -- keep scores equal to the current max
                 else s <= best   -- keep scores equal to the current min
            ) xs


-- Get best fen found in the tree after it is pruned and return it
getBestFEN :: GameTree -> String
getBestFEN (Node _ children) =
  let scored = [(fen, s) | Node (fen, s) _ <- children]
      bestScore = maximum (map snd scored)
  in fst $ head $ filter (\(_, s) -> s == bestScore) scored


-- Given a starting and resulting FEN, return the (from, to) move as coordinates
getMoveCoords :: String -> String -> ((Int, Int), (Int, Int))
getMoveCoords fen1 fen2 = (getCoordinateMoved b1 b2, getNewCoordinate b1 b2)
  where
    b1 = ChessFunctions.boardFromPosition (positionFromFEN fen1)
    b2 = ChessFunctions.boardFromPosition (positionFromFEN fen2)


-- Construct tree and return the best found move (tree-based minimax)
aiMoveTree :: Int -> String -> ((Int, Int), (Int, Int))
aiMoveTree d fen =
  let tree = buildTree d fen
      bestFen = getBestFEN tree
  in getMoveCoords fen bestFen

-- | Best move using on‑the‑fly alpha‑beta pruning (no full tree build)
aiMove :: Int -> String -> ((Int, Int), (Int, Int))
aiMove d fen =
  let moves  = ChessFunctions.getLegalMoves fen
      scored = map (\m -> (- alphaBeta (d - 1) False m (-inf) inf, m)) moves
      bestFen = snd $ maximumBy (compare `on` fst) scored
  in getMoveCoords fen bestFen

--Debug search function to see if a move exists in the game tree using a current FEN
searchGT :: String -> GameTree -> Bool
searchGT targetFEN (Node (fen, _) children)
  | fen == targetFEN = True
  | otherwise = any (searchGT targetFEN) children

--Debug function to test the search function
testFENTree :: GameTree
testFENTree =
  Node ("8/8/8/8/8/8/3k4/3K4 w - - 0 1", 0)
    [ Node ("8/8/8/8/8/8/3k4/2K5 b - - 1 1", 0)  -- Kd1-c2
        [ Node ("8/8/8/8/8/8/4k3/2K5 w - - 2 2", -500) [] ]  -- Kd2-e2
    , Node ("8/8/8/8/8/8/3k4/4K3 b - - 1 1", 0)  -- Kd1-e1
        [ Node ("8/8/8/8/8/8/4k3/4K3 w - - 2 2", -500) [] ]  -- Kd2-e2
    ]