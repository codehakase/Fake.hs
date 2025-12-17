module Fake.Combinators (
    vector,
    vectorOf,
    sequence,
    maybeGen,
    oneof,
    filterGen,
    mapGen,
    flatMap,
) where

import Fake.Core (Fake)
import Fake.Primitives (bool, integerRange)
import Prelude hiding (filter, map, sequence)

vector :: Int -> Fake a -> Fake [a]
vector n gen
    | n <= 0 = return []
    | otherwise = sequence $ replicate n gen

vectorOf :: (Int, Int) -> Fake a -> Fake [a]
vectorOf (minLen, maxLen) gen = do
    count <- integerRange minLen maxLen
    vector count gen

sequence :: [Fake a] -> Fake [a]
sequence [] = return []
sequence (x : xs) = do
    val <- x
    rest <- sequence xs
    return (val : rest)

maybeGen :: Fake a -> Fake (Maybe a)
maybeGen gen = do
    shouldGen <- bool
    if shouldGen
        then Just <$> gen
        else return Nothing

oneof :: [Fake a] -> Fake a
oneof [] = error "oneof: empty list"
oneof gens = do
    idx <- integerRange 0 (length gens - 1)
    gens !! idx

filterGen :: (a -> Bool) -> Fake a -> Fake a
filterGen p gen = do
    val <- gen
    if p val
        then return val
        else filterGen p gen

mapGen :: (a -> b) -> Fake a -> Fake b
mapGen f gen = f <$> gen

flatMap :: (a -> Fake b) -> Fake a -> Fake b
flatMap f gen = gen >>= f
