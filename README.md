# Declarative Chess

### Chess: A Brief History

Chess is a classic board game that's been around for like, a long time. It has origins in India in a previous version of the game known as Chaturanga, which has been around for over 1500 years. Chess has had a long time to refine the rules, so let's take a look at them.

### Chess: A Basic Look at the Rules
Chess is a two player strategy game in which players take turns moving pieces across a square board of 64 squares. Each piece occupies a square and depending on the type of piece can move in specific geometric patterns. Pieces can 'capture' other pieces to remove them from the board. The object of the game is to put the opponent's king piece into a position in which capture is inevitable (checkmate).

### Chess: How To Move The Pieces

#### The Pawn:
The pawn may be the most common piece, but it is far from the simplest. The basic movement of the pawn consists of the following: It can move forward (different for white or black) one square at a time, or it can capture diagonally forward 1 square. It cannot move diagonally, or capture vertically, so it's movement and captures are entirely separate. Some weird things about the pawn: On it's starting square, it can choose to move forward 2 instead of 1. But be warned, if you pass another pawn's line of attack in this way, then for one turn immediately after the opposing pawn can capture your pawn as if it had only moved 1 square. This is known as "en passant", which is french for nerd (in passing). Also, when the pawn reaches the opposing back rank, it can "promote" to a knight, bishop, rook or queen of the same color. For this program, it just promotes to a queen.

#### The Knight:
The knight (horsey) is one of the trickiest pieces to get the hang of, due to its slightly unusual geometry. The knight moves in an 'L' shape, 1 orthogonally in 1 direction, followed by 2 orthogonally in the other direction. Also, it can "jump" over pieces, a trait unique to the knight. Honestly, it's pretty hard to explain this one. Just click on the horsey and see the squares that light up, and you'll get the idea.

#### The Bishop:
The bishop is a relatively simple piece. It can move diagonally. It cannot jump over pieces. It has "infinite" range.

#### The Rook:
That's the castle shaped one. This one is also pretty simple. It can move horizontally and vertically. It cannot jump over pieces. It has "infinite" range. There's also a special move that can be performed by the king that involves the rook, and the ability to do that move relies on the rook not having been moved yet. So just look out for that.

#### The Queen:
Everyone wants to be her. This one can move like the rook or the bishop. Pretty overpowered. But don't get too excited, if you lose her you're basically screwed, so just be careful.

#### The King:
The king is surprisingly(?) limited. It can move only 1 square in any direction, orthogonal or diagonal. If your king is threatened, this is known as "check" and you must play a move that prevents the capture of your king. That also means you can't play a move that results in your king being in check. If you cannot escape the check, that's checkmate and you lose. If all of your moves would put you in check but you're not actively in check, that's actually a draw (known as stalemate). Also, the king can do a funny trick, it's called castling. If the king hasn't moved and the desired rook hasn't moved, and all the squares between them are vacant and the two squares towards the rook from the king are unattacked and the king is unattacked, then the king can move two squares towards the rook instead of just one, and the rook moves to the other side of the king. If it seems like a lot to keep track of, you can always just click on the king and see which squares light up.

### Check, Checkmate, Stalemate:
#### Check:
When the king is under threat this is known as check. You cannot play a move that would result in your king being in check.

#### Checkmate:
If your king is in check and you have no legal moves (ways to stop the attack), then it's checkmate and you lose.

#### Stalemate:
If your king isn't in check but you have no legal moves, then it's stalemate and it's a draw.

## Our Program
Our program has the entirety of the chess rules, and it also detects legal moves given any square you select. So you can click on a piece and see where it can go, pretty convenient! You can play a complete chess game, and you can even play with a computer player.

### Chess Logic:
Valerie Barker was responsible for the chess logic, including parsing chess positions from FEN strings, finding legal moves and piece movement of all the pieces, including special cases like en passant, castling and promotion. She also made game over detection and helped with the GUI. Youssef Amin also helped with the chess logic, making the legal move detection for bishop and queen.

### GUI:
Youssef Amin was responsible for the GUI including handling player clicks and generating the next game state for any given moment, and drawing the Board. Alex Maynes made the sprites for the GUI and helped with a lot of helper functions for the GUI. Valerie Barker also helped with the GUI.

### Computer Player:
Alex Maynes was responsible for the computer player.

## How To Run:
navigate to DeclarativeChess-357 in the terminal and do 

```cabal run```

From there you can start playing! Click on the pieces and the desired squares to move them. Be patient for the computer player. 

You can enable or disable the computer player to play human vs human or human vs computer by changing the boolean value in TestHandleEvent in updateClickState