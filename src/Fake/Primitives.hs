module Fake.Primitives (
    integer,
    integerRange,
    natural,
    float,
    double,
    floatInRange,
    doubleInRange,
    bool,
    char,
    alpha,
    numeric,
    alphanumeric,
    string,
    stringBounded,
    elements,
    shuffle,
) where

import Control.Monad (replicateM)
import Control.Monad.State (state)
import Fake.Core (Fake, liftState)
import System.Random (random, randomR)

integer :: Fake Int
integer = liftState $ state random

integerRange :: Int -> Int -> Fake Int
integerRange lo hi = liftState $ state (randomR (lo, hi))

natural :: Fake Int
natural = abs <$> integer

float :: Fake Float
float = liftState $ state random

double :: Fake Double
double = liftState $ state random

scaledRange :: (Ord a, Num a) => Fake a -> a -> a -> Fake a
scaledRange gen lo hi
    | lo > hi = error "scaledRange: lo > hi"
    | otherwise = do
        val <- gen
        return $ lo + val * (hi - lo)

doubleInRange :: Double -> Double -> Fake Double
doubleInRange = scaledRange double

floatInRange :: Float -> Float -> Fake Float
floatInRange = scaledRange float

bool :: Fake Bool
bool = liftState $ state random

char :: Fake Char
char = liftState $ state random

alpha :: Fake Char
alpha = elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

numeric :: Fake Char
numeric = elements "0123456789"

alphanumeric :: Fake Char
alphanumeric = elements "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

string :: Int -> Fake String
string n = replicateM n alphanumeric

stringBounded :: (Int, Int) -> Fake String
stringBounded (minLen, maxLen)
    | minLen > maxLen = error "stringBounded: minLen > maxLen"
    | minLen < 0 = error "stringBounded: minLen < 0"
    | otherwise = do
        len <- integerRange minLen maxLen
        string len

elements :: [a] -> Fake a
elements [] = error "elements: empty list"
elements xs = do
    idx <- integerRange 0 (length xs - 1)
    return $ xs !! idx

shuffle :: [a] -> Fake [a]
shuffle [] = return []
shuffle xs = do
    -- Fisher-Yates shuffle algorithm
    let len = length xs
    shuffleHelper xs len
  where
    shuffleHelper ys 0 = return ys
    shuffleHelper ys n = do
        idx <- integerRange 0 (n - 1)
        let (left, right) = splitAt idx ys
        let (a, b) = case right of
                [] -> error "shuffle: internal error"
                (x : rest) -> (x, left ++ rest)
            newYs = a : b
        shuffleHelper newYs (n - 1)
