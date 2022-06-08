module UI where

import Chess

import Brick
import Graphics.Vty.Attributes

runUI :: IO ()
runUI = do
    _ <- defaultMain app initialGame
    return ()

type Event = ()
type Name = ()

app :: App Game Event Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          } 

drawUI :: Game -> [Widget Name]
drawUI g = [vBox . printBoard $ board g]

printBoard :: Board -> [Widget Name]
printBoard b = printRow <$> b where
    printRow xs = str . foldr (++) "" $ printCell <$> xs

    printCell Nothing       = " "
    printCell (Just (c, p)) = printPiece c p

printPiece :: Colour -> Piece -> String
printPiece White p = printWhitePiece p where
    printWhitePiece King = "♔"
    printWhitePiece Queen = "♕"
    printWhitePiece Rook = "♖"
    printWhitePiece Bishop = "♗"
    printWhitePiece Knight = "♘"
    printWhitePiece Pawn = "♙"
printPiece Black p = printBlackPiece p where
    printBlackPiece King = "♚"
    printBlackPiece Queen = "♛"
    printBlackPiece Rook = "♜"
    printBlackPiece Bishop = "♝"
    printBlackPiece Knight = "♞"
    printBlackPiece Pawn = "♟"

handleEvent :: Game -> BrickEvent Name Event -> EventM Name (Next Game)
handleEvent = undefined

theMap :: AttrMap
theMap = attrMap defAttr []
