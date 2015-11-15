module Game(Game(..), Rule, Move, gameMoves, gameMakeMove, gameIsWon, gameNoMoves, gameNew, cellToMove, winCombs) where

import Board

data Game = Game
    { gameBoard   :: Board
    , gameRules   :: [Rule]
    , gameXO      :: XO
    , gameHistory :: [Cell]
    }

type CellComb = [XO]
type Rule     = Board -> [CellComb]
type Move     = (Int, Int)

cellToMove :: Cell -> Move
cellToMove (x, y, _) = (x, y)

defaultRulesGen :: Int -> [Rule]
defaultRulesGen maxs =
    -- Rule (x, y) -> (x + 1, y)
    [ \(Board w h cells) -> let rule (x, y) = [cells !! ((x+s) + w*y) | s <- [0 .. maxs - 1]]
                            in  map rule [(x, y) | x <- [0 .. w - maxs], y <- [0 .. h - 1]]
    -- Rule (x, y) -> (x, y + 1)
    , \(Board w h cells) -> let rule (x, y) = [cells !! (x + w*(y+s)) | s <- [0 .. maxs - 1]]
                            in  map rule [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - maxs]]
    -- Rule (x, y) -> (x + 1, y + 1)
    , \(Board w h cells) -> let rule (x, y) = [cells !! (x' + w*y') | s <- [0 .. maxs - 1], let x' = x + s, let y' = y + s]
                            in  map rule [(x, y) | x <- [0 .. w - maxs], y <- [0 .. h - maxs]]
    -- Rule (x, y) -> (x - 1, y + 1)
    , \(Board w h cells) -> let rule (x, y) = [cells !! (x' + w*y') | s <- [0 .. maxs - 1], let x' = x - s, let y' = y + s]
                            in  map rule [(x, y) | x <- [maxs - 1 .. w - 1], y <- [0 .. h - maxs]]
    ]

cellCombs :: Board -> [Rule] -> [CellComb]
cellCombs b rs = concatMap (\r -> r b) $ rs

winCombs :: Board -> [Rule] -> [CellComb]
winCombs b rs = filter conditions $ cellCombs b rs
    where
        conditions = do
            c1 <- not . null
            c2 <- not . any (== N)
            c3 <- \(c:cx) -> all (== c) cx
            return $ c1 && c2 && c3

gameWinCombs :: Game -> [CellComb]
gameWinCombs (Game b rs _ _) = winCombs b rs

gameMakeMove :: Game -> Move -> Maybe Game
gameMakeMove (Game b rs xo h) (x, y) = do
    let c = (x, y, xo)
    mb <- boardMakeMove b c
    return $ Game mb rs (oppositeXO xo) (c:h)

gameIsWon :: Game -> Bool
gameIsWon = not . null . gameWinCombs

gameMoves :: Game -> [Move]
gameMoves = map cellToMove . boardNCells . gameBoard

gameNoMoves :: Game -> Bool
gameNoMoves = null . boardNCells . gameBoard

gameNew :: Game
gameNew = Game (boardNew 3 3) (defaultRulesGen 3) N []