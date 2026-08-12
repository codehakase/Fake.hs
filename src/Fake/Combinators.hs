module Fake.Combinators (
    vector,
    vectorOf,
    maybeGen,
    oneof,
    filterGen,
) where

import Control.Monad (join, replicateM)
import Fake.Core (Fake)
import Fake.Primitives (bool, elements, integerRange)

vector :: Int -> Fake a -> Fake [a]
vector = replicateM

vectorOf :: (Int, Int) -> Fake a -> Fake [a]
vectorOf (minLen, maxLen) gen = do
    count <- integerRange minLen maxLen
    vector count gen

maybeGen :: Fake a -> Fake (Maybe a)
maybeGen gen = do
    shouldGen <- bool
    if shouldGen
        then Just <$> gen
        else return Nothing

oneof :: [Fake a] -> Fake a
oneof gens = join (elements gens)

filterGen :: (a -> Bool) -> Fake a -> Fake a
filterGen p gen = do
    val <- gen
    if p val
        then return val
        else filterGen p gen
