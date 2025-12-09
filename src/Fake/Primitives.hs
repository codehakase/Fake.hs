module Fake.Primitives
  ( integer
  , integerRange
  , natural
  , float
  , double
  , bool
  , char
  , alpha
  , numeric
  , alphanumeric
  , string
  , stringBounded
  , elements
  , shuffle
  ) where

import Fake.Core (Fake, liftState)
import Control.Monad.State (state)
import System.Random (randomR, random)

integer :: Fake Int
integer = liftState $ state random

integerRange :: Int -> Int -> Fake Int
integerRange lo hi = liftState $ state (randomR (lo, hi))

natural :: Fake Int
natural = do
  n <- integer
  return $ abs n

float :: Fake Float
float = liftState $ state random

double :: Fake Double
double = liftState $ state random

bool :: Fake Bool
bool = liftState $ state random

char :: Fake Char
char = liftState $ state random

alpha :: Fake Char
alpha = do
  let alphas = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  elements alphas

numeric :: Fake Char
numeric = do
  let digits = "0123456789"
  elements digits

alphanumeric :: Fake Char
alphanumeric = do
  let chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  elements chars

string :: Int -> Fake String
string n
  | n <= 0 = return ""
  | otherwise = sequence $ replicate n alphanumeric

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
shuffle xs = return xs
