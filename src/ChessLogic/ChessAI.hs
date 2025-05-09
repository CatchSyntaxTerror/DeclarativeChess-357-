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

prune :: GameTree -> Int -> Int -> Bool -> Int
prune (Node (_, score) []) _ _ _ = score
prune (Node _ children) alpha beta maximizingPlayer =
  if maximizingPlayer
    then maxValue children alpha beta
    else minValue children alpha beta
  where
    maxValue [] a _ = a
    maxValue (n:ns) a b =
      let a' = max a (prune n a b False)
      in if a' >= b
           then a'
           else maxValue ns a' b
    minValue [] _ b = b
    minValue (n:ns) a b =
      let b' = min b (prune n a b True)
      in if a >= b'
           then b'
           else minValue ns a b'

--Debug search function to see if a move exists in the game tree using a current FEN
searchGT :: String -> GameTree -> Bool
searchGT targetFEN (Node (fen, _) children)
  | fen == targetFEN = True
  | otherwise = any (searchGT targetFEN) children