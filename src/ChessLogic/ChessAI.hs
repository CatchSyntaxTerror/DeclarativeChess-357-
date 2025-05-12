module ChessLogic.ChessAI where

import ChessLogic.ChessFunctions as ChessFunctions
  
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
  | c == 'P'  = pVal pawn
  | c == 'p'  = -(pVal pawn)
  | c == 'N'  = pVal knight
  | c == 'n'  = -(pVal knight)
  | c == 'B'  = pVal bishop
  | c == 'b'  = -(pVal bishop)
  | c == 'R'  = pVal rook
  | c == 'r'  = -(pVal rook)
  | c == 'Q'  = pVal queen
  | c == 'q'  = -(pVal queen)
  | c == 'K'  = pVal king
  | c == 'k'  = -(pVal king)
  | c `elem` ['1'..'8'] = 0 -- Empty squares
  | c == '/' = 0           -- Ignore rank separators
  | otherwise = 0          -- Safety fallback

-- Chess game tree depth value
depth :: Int
depth = 10

-------------------------------------------------------------------

-- Game Tree Data Type (FEN, Material Score) [Child Nodes]
data GameTree = Node (String, Int) [GameTree]
  deriving (Show, Eq)

-- Create chess game tree with 1st node being the material score-maximizing node
buildTree :: Int -> String -> GameTree
buildTree h fen = buildTree' h True fen

buildTree' :: Int -> Bool -> String -> GameTree
buildTree' 0 maximizing fen = Node (fen, generateMaterialScore maximizing fen []) []
buildTree' h maximizing fen =
  let children = generateChildren (h - 1) (not maximizing) (fenTOfens fen)
  in Node (fen, generateMaterialScore maximizing fen children) children

-- Generate Child Nodes for a Parent Node aka generate all possible moves for a given FEN
generateChildren :: Int -> Bool -> [String] -> [GameTree]
generateChildren h maximizing fens = map (buildTree' h maximizing) fens

-- Take a FEN and returns an array of FENs aka an array of all possible moves
fenTOfens :: String -> [String]
fenTOfens fen = getLegalMoves fen

-- Evaluate an FEN and generate it's material score
generateMaterialScore :: Bool -> String -> [GameTree] -> Int
generateMaterialScore maximizingPlayer fen children = 
  if children == []
    then sum (map charValue (takeWhile (/= ' ') fen))
    else minimax maximizingPlayer children

--Minimax of all child nodes based on the player turn
minimax :: Bool -> [GameTree] -> Int
minimax maximizingPlayer children =
  let scores = map (\(Node (_, s) _) -> s) children
  in if maximizingPlayer
       then maximum scores
       else minimum scores

-- Recursively prune all paths that do not match the minimax-optimal score
pruneTree :: Bool -> GameTree -> GameTree
pruneTree maximizing (Node (fen, score) []) = Node (fen, score) []
pruneTree maximizing (Node (fen, score) children) =
  let prunedChildren = map (pruneTree (not maximizing)) children
      bestScore = minimax maximizing prunedChildren
      filtered = filter (\(Node (_, s) _) -> s == bestScore) prunedChildren
  in Node (fen, score) filtered

-- Get best fen found in the tree after it is pruned and return it
getBestFEN :: GameTree -> String
getBestFEN (Node _ children) =
  let scored = [(fen, s) | Node (fen, s) _ <- children]
      bestScore = maximum (map snd scored)
  in fst $ head $ filter (\(_, s) -> s == bestScore) scored

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