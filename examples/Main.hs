module Main where

import Fake
import qualified Fake.Person as Person
import qualified Fake.Internet as Internet
import qualified Fake.Address as Address

main :: IO ()
main = do
  putStrLn "=== fake-hs Examples ==="
  putStrLn ""

  putStrLn "1. Deterministic generation (seeded):"
  let name = runFakerSeed 42 Person.fullName
  putStrLn $ "   Name: " ++ name
  putStrLn ""

  putStrLn "2. Generating a user profile:"
  let user = runFakerSeed 123 generateUser
  putStrLn $ "   Name: " ++ userName user
  putStrLn $ "   Email: " ++ userEmail user
  putStrLn $ "   Age: " ++ show (userAge user)
  putStrLn $ "   City: " ++ userCity user
  putStrLn ""

  putStrLn "3. Reproducibility check:"
  let user1 = runFakerSeed 999 generateUser
  let user2 = runFakerSeed 999 generateUser
  putStrLn $ "   Email 1: " ++ userEmail user1
  putStrLn $ "   Email 2: " ++ userEmail user2
  putStrLn $ "   Same? " ++ show (userEmail user1 == userEmail user2)
  putStrLn ""

  putStrLn "4. Non-deterministic generation (system random):"
  randomName <- runFaker Person.fullName
  randomEmail <- runFaker Internet.email
  putStrLn $ "   Name: " ++ randomName
  putStrLn $ "   Email: " ++ randomEmail
  putStrLn ""

  putStrLn "5. Combinators in action:"
  let city = runFakerSeed 555 Address.city
  let country = runFakerSeed 666 Address.country
  let url = runFakerSeed 777 Internet.url
  putStrLn $ "   City: " ++ city
  putStrLn $ "   Country: " ++ country
  putStrLn $ "   URL: " ++ url

data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userCity :: String
  } deriving (Show)

generateUser :: Fake User
generateUser = do
  n <- Person.fullName
  e <- Internet.email
  a <- integerRange 18 80
  c <- Address.city
  return $ User n e a c
