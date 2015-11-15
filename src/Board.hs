module Board where

import Data.List     (unfoldr)
import System.Random (RandomGen)
import Utils         (splitEvery, randomListEl)

data XO     = N | X | O deriving (Eq, Ord, Show)
data Board  = Board Int Int [XO] deriving Show
type Cell   = (Int, Int, XO)

boardNew :: Int -> Int -> Board
boardNew w h = Board w h . replicate (h * w) $ N

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
boardRandom :: RandomGen g => Int -> Int -> g -> Board
boardRandom w h = Board w h . take (w * h) . unfoldr (Just . randomListEl [N, X, O])

oppositeXO X = O
oppositeXO O = X
oppositeXO N = N

cellXO :: Cell -> XO
cellXO (_, _, xo) = xo

oneCell :: [[XO]] -> Int -> Int -> Cell
oneCell cells x y = (x, y, cells !! y !! x)

coordsToIndex (Board w _ _) x y = x + y * w

boardXOUnsafe :: Board -> Int -> Int -> XO
boardXOUnsafe (Board w _ xos) x y = xos !! (x + y * w)

boardCellUnsafe :: Board -> Int -> Int -> Cell
boardCellUnsafe b@(Board _ _ xos) x y = (x, y, xos !! coordsToIndex b x y)

boardCells :: Board -> [Cell]
boardCells b@(Board w h _) = [ boardCellUnsafe b x y | x <- [0..(w-1)], y <- [0..(h-1)]]

boardNCells :: Board -> [Cell]
boardNCells = filter ((N==) . cellXO) . boardCells

boardCoordIsValid :: Board -> Int -> Int -> Bool
boardCoordIsValid (Board w h _) x y = x < 0 || y < 0 || x >= w || y >= h

boardCell :: Board -> Int -> Int -> Maybe Cell
boardCell b x y
    | boardCoordIsValid b x y = Nothing
    | otherwise = Just $ (x, y, boardXOUnsafe b x y)

boardSetCellUnsafe :: Board -> Cell -> Board
boardSetCellUnsafe b@(Board w h xos) (x, y, xo) =
    let i = coordsToIndex b x y
    in  Board w h $ take i xos ++ [xo] ++ drop (i + 1) xos

boardMakeMove :: Board -> Cell -> Maybe Board
boardMakeMove b (x, y, xo)
    | xo == N = Nothing
    | boardCoordIsValid b x y = Nothing
    | boardXOUnsafe b x y /= N = Nothing
    | otherwise = Just $ boardSetCellUnsafe b (x, y, xo)