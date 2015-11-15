module Bot where

import Data.List        (mapAccumL)
import Data.Maybe       (fromJust, catMaybes)
import Control.Monad    (msum)
import qualified Data.Map as M

import Utils            (average)
import Board
import Game

type Prob = (Move, Rational)
type Probs = [Prob]

-- X - игрок, O - противник
type ProbCache = M.Map [XO] Probs
cacheKey :: XO -> Board -> [XO]
cacheKey xo (Board _ _ cells) = map norm cells
    where
        norm a | a == N     = N
               | a == xo    = X
               | otherwise  = O

gameBotMoves :: Game -> [Move]
gameBotMoves g = fromJust $ msum
                    [ maybefy $ gameProbWinMoves g
                    , maybefy $ gameDrawMoves g
                    , maybefy $ gameMoves g
                    , Just [] ]
                 where
                     maybefy [] = Nothing
                     maybefy l = Just l

gameDrawMoves :: Game -> [Move]
gameDrawMoves (Game b rs xo _) = map cellToMove $ boardDrawMoves b rs xo

gameWinMoves :: Game -> [Move]
gameWinMoves (Game b rs xo _) = map cellToMove $ boardWinMoves b rs xo

gameProbWinMoves :: Game -> [Move]
gameProbWinMoves (Game b rs xo _) =
    let probWins = snd $ boardProbWinMoves b rs xo M.empty
        bestProb = maximum (map snd probWins)
        bestChoices = filter ((bestProb ==) . snd) probWins
    in  map fst bestChoices

_gameProbWinMoves (Game b rs xo _) = boardProbWinMoves b rs xo

boardIsWon :: Board -> [Rule] -> Bool
boardIsWon b rs = not . null . winCombs b $ rs

boardDrawMoves :: Board -> [Rule] -> XO -> [Cell]
boardDrawMoves b rs xo = do
    move <- getMoves xo b
    let eb      = boardSetCellUnsafe b move
        eMoves  = getMoves (oppositeXO xo) eb
    if boardIsWon eb rs || null eMoves then
        return move
    else
        let pbs = map (boardSetCellUnsafe eb) eMoves
            -- Хотя бы один из ходов противника приводит к победе
        in  if any (\b -> boardIsWon b rs) pbs then
                []
            -- Ход противника был единственным и, с-но, последним
            else if drop 1 eMoves == [] then
                return move
            -- При любом ходе противника есть ход, сводящий игру хотя бы в ничью
            else if all (not . null) $ map (\b -> boardDrawMoves b rs xo) pbs then
                return move
            -- Противник может походить так, что он непременно выиграет
            else
                []
    where
        getMoves :: XO -> Board -> [Cell]
        getMoves xo = map (\(x, y, _) -> (x, y, xo)) . boardNCells

boardWinMoves :: Board -> [Rule] -> XO -> [Cell]
boardWinMoves b rs xo = do
    move <- getMoves xo b
    let eb      = boardSetCellUnsafe b move
        eMoves  = getMoves (oppositeXO xo) eb
    if boardIsWon eb rs then
        return move
    else if null eMoves then
        []
    else
        let pbs = map (boardSetCellUnsafe eb) eMoves
            -- Хотя бы один из ходов противника приводит к победе
        in  if any (\b -> boardIsWon b rs) pbs then
                []
            -- Ход противника был единственным и, с-но, последним
            else if drop 1 eMoves == [] then
                []
            -- При любом ходе противника есть ход, приводящий к победе игрока
            else if all (not . null) $ map (\b -> boardWinMoves b rs xo) pbs then
                return move
            -- Противник может походить так, что он непременно выиграет
            else
                []
    where
        getMoves :: XO -> Board -> [Cell]
        getMoves xo = map (\(x, y, _) -> (x, y, xo)) . boardNCells

boardProbWinMoves :: Board -> [Rule] -> XO -> ProbCache -> (ProbCache, Probs)
boardProbWinMoves b rs xo cache = 
    let ckey = cacheKey xo b
        moves = getMoves xo b
        result = mapAccumL eachMove cache moves
        resProbs = catMaybes $ snd result
        resCache = fst result
    in  if      ckey `M.member` cache
        then    (cache, cache M.! ckey)
        else    (M.insert ckey resProbs resCache, resProbs)
    where
        getMoves :: XO -> Board -> [Cell]
        getMoves xo = map (\(x, y, _) -> (x, y, xo)) . boardNCells
        eachMove :: ProbCache -> Cell -> (ProbCache, Maybe Prob)
        eachMove cache cell =
            let eb      = boardSetCellUnsafe b cell
                eMoves  = getMoves (oppositeXO xo) eb
            -- Ход игрока приводит к победе
            in  if boardIsWon eb rs then
                    (cache, Just (cellToMove cell, 1))
                -- Ход игрока был последним
                else if null eMoves then
                    (cache, Just (cellToMove cell, 0))
                else
                    let pbs :: [Board]
                        pbs = map (boardSetCellUnsafe eb) eMoves
                        -- Хотя бы один из ходов противника приводит к победе
                    in  if any (\b -> boardIsWon b rs) pbs then
                            (cache, Nothing)
                        -- Ход противника был единственным и, с-но, последним
                        else if drop 1 eMoves == [] then
                            (cache, Just (cellToMove cell, 0))
                        else
                            let result :: (ProbCache, [Probs])
                                result = mapAccumL eachBoard cache pbs
                                eachBoard cache b = boardProbWinMoves b rs xo cache

                                winMoves :: [Probs]
                                winMoves = snd result
                                winProb = average . map (average . map snd) $ winMoves
                                -- При любом ходе противника есть ход, сводящий игру хотя бы в ничью
                            in  if all (not . null) $ winMoves then
                                    (fst result, Just (cellToMove cell, winProb))
                                -- Противник может походить так, что он непременно выиграет
                                else
                                    (cache, Nothing)