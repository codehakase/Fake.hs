{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fake.Core (
    Fake,
    runFaker,
    runFakerSeed,
    runFakerWith,
    liftState,
) where

import Control.Monad.State (State, evalState)
import System.Random (StdGen, initStdGen, mkStdGen)

newtype Fake a = Fake (State StdGen a)
    deriving (Functor, Applicative, Monad)

runFaker :: Fake a -> IO a
runFaker (Fake computation) = do
    gen <- initStdGen
    return $ evalState computation gen

runFakerSeed :: Int -> Fake a -> a
runFakerSeed seed (Fake computation) =
    let gen = mkStdGen seed
     in evalState computation gen

runFakerWith :: StdGen -> Fake a -> a
runFakerWith gen (Fake computation) = evalState computation gen

liftState :: State StdGen a -> Fake a
liftState st = Fake st
