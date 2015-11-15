module Utils where

import Control.Applicative
import Control.Monad
import System.Random

unfoldr :: (b -> Maybe (a, b)) -> b -> Int -> Maybe [a]
unfoldr _ _ 0 = Just []
unfoldr f z c = case f z of
    Just (a, b) -> (a:) <$> unfoldr f b (c - 1)
    Nothing -> Nothing

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
generate :: (a -> Maybe a) -> a -> Int -> Maybe [a]
generate f z c = unfoldr (f >=> tuplify) z (c - 1) >>= Just . (z:)
    where tuplify = \x -> Just (x, x)

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomListEl :: RandomGen g => [a] -> g -> (a, g)
randomListEl l = (\(a, g) -> (l !! a, g)) . randomR (0, length l - 1)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

countOcc :: (Eq a, Num b) => [a] -> [(a, b)]
countOcc = foldl f []
    where
        f [] z = [(z, 1)]
        f ((v, c):acc) z | v == z    = (z, c+1):acc
                         | otherwise = (v,c) : (f acc z)

average :: [Rational] -> Rational
average = (\(s,c) -> s/c) . foldl (\(s, c) z -> (s+z, c+1)) (0, 0)