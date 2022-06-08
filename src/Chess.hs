module Chess 
    ( Game(board, winner, turn), initialGame, Board, Cell, Piece(..), Colour(..)
    )where

data Game = Game
    { board :: Board
    , winner :: Maybe Colour
    , turn :: Colour
    } deriving (Show)

type Board = [[Cell]]
type Cell = Maybe (Colour, Piece)

data Piece
    = King
    | Queen
    | Bishop
    | Knight
    | Rook
    | Pawn
    deriving (Show)

data Colour
    = White
    | Black
    deriving (Show)

initialGame :: Game
initialGame = Game initialBoard Nothing White

initialBoard :: Board 
initialBoard = whitePieces ++ emptyRows ++ blackPieces where
    whitePieces = initialPieces White
    emptyRows = take 4 . repeat . take 8 . repeat $ Nothing
    blackPieces = reverse $ initialPieces Black

initialPieces :: Colour -> Board 
initialPieces c = (f <$>) <$> pieces where
    f = \piece -> Just (c, piece)
    pieces = [ [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook],
               take 8 (repeat Pawn) ]
