module ChessLogic.ChessAI where
  
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

-- Create chess game tree
buildTree :: Int -> String -> GameTree
buildTree 0 fen = Node (fen, generateMaterialScore fen) []
buildTree d fen = Node (fen, generateMaterialScore fen) (generateChildren (d-1) fen)

-- Generate Child Nodes for a Parent Node aka generate all possible moves for a given FEN
generateChildren :: Int -> String -> [GameTree]
generateChildren 0 _ = []
generateChildren d fen = map (buildTree (d-1)) (fenTOfens fen)

-- Take a FEN and returns an array of FENs aka an array of all possible moves
fenTOfens :: String -> [String]
fenTOfens = undefined

-- Evaluate an FEN and generate it's material score
generateMaterialScore :: String -> Int
generateMaterialScore fen = sum (map charValue (takeWhile (/= ' ') fen))

--Prune out paths not better and the same (IN WORKS STILL)
prune :: GameTree -> Int -> Int -> Bool -> GameTree
prune (Node (fen, score) []) _ _ _ = Node (fen, score) []
prune (Node (fen, score) children) alpha beta maximizingPlayer =
  if maximizingPlayer
    then let (bestChild, _) = pruneMax children alpha beta
         in Node (fen, score) [bestChild]
    else let (bestChild, _) = pruneMin children alpha beta
         in Node (fen, score) [bestChild]
  where
    pruneMax [] a _ = (Node ("", -5000) [], a)
    pruneMax (n:ns) a b =
      let nPruned@(Node (fenN, s) childrenN) = prune n a b False
          a' = max a s
      in if a' >= b
           then (nPruned, a')
           else
             let (best, bestVal) = pruneMax ns a' b
             in if s >= bestVal then (nPruned, s) else (best, bestVal)

    pruneMin [] _ b = (Node ("", 5000) [], b)
    pruneMin (n:ns) a b =
      let nPruned@(Node (fenN, s) childrenN) = prune n a b True
          b' = min b s
      in if a >= b'
           then (nPruned, b')
           else
             let (best, bestVal) = pruneMin ns a b'
             in if s <= bestVal then (nPruned, s) else (best, bestVal)

--Debug search function to see if a move exists in the game tree using a current FEN
searchGT :: String -> GameTree -> Bool
searchGT targetFEN (Node (fen, _) children)
  | fen == targetFEN = True
  | otherwise = any (searchGT targetFEN) children

--Test tree
testFENTree :: GameTree
testFENTree =
  Node ("8/8/8/8/8/8/3k4/3K4 w - - 0 1", 0)
    [ Node ("8/8/8/8/8/8/3k4/2K5 b - - 1 1", 0)  -- Kd1-c2
        [ Node ("8/8/8/8/8/8/4k3/2K5 w - - 2 2", -500) [] ]  -- Kd2-e2
    , Node ("8/8/8/8/8/8/3k4/4K3 b - - 1 1", 0)  -- Kd1-e1
        [ Node ("8/8/8/8/8/8/4k3/4K3 w - - 2 2", -500) [] ]  -- Kd2-e2
    ]