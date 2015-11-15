module Main where

import Data.Maybe
import System.Random (RandomGen, newStdGen)
import Control.Applicative
import Utils (randomListEl, average)
import Data.Ratio

import Board
import Game
import Bot

readInt :: String -> Int
readInt = read

outputGame :: Game -> IO ()
outputGame = putStrLn . concatMap ( (++ "\n") . show) . (\(Board _ _ cells) -> cells) . gameBoard

mainUI :: RandomGen g => g -> Game -> IO()
mainUI g game = do
    outputGame game
    print $ "Turn of " ++ show (gameXO game)
    print "Input command:"
    cmd <- getLine
    case words cmd of
        ["exit"] -> return ()
        ["restart"] -> main
        ["put", x, y] -> do
            let nx      = readInt x
                ny      = readInt y
                mGame   = gameMakeMove game (nx-1, ny-1)
                nGame   = fromJust mGame
            if isNothing mGame then do
                print "Wrong coordinates"
                mainUI g game
            else
                afterMoveUI g (gameXO game) nGame
        ["bot"] -> do
            let (move, ng)  = randomListEl (gameBotMoves game) g
                nGame       = fromJust $ gameMakeMove game move
            afterMoveUI ng (gameXO game) nGame

        _ -> do
            print "unknown command"
            mainUI g game

afterMoveUI :: RandomGen g => g -> XO -> Game -> IO ()
afterMoveUI g xo game =
    if gameIsWon game then do
        print $ "The " ++ show xo ++ " wins!"
        print $ show (gameBoard game)
    else if gameNoMoves game then do
        print $ "The game ended in a draw, both players are loosers!"
        print $ show (gameBoard game)
    else
        mainUI g game

-- tb = Board 3 3 [[N,X,N], [O,O,N], [N,N,N]]

-- | The main entry point.
main :: IO ()
main = do
    g <- newStdGen
    let (xo, ng) = randomListEl [X,O] g
    mainUI ng gameNew { gameXO = xo }