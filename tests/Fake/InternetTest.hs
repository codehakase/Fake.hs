module Fake.InternetTest (internetTests) where

import Test.Hspec
import Fake
import qualified Fake.Internet as Internet
import Data.Char (isLower)

internetTests :: Spec
internetTests = describe "Faker.Internet" $ do
  it "username is non-empty" $ do
    let username = runFakerSeed 100 Internet.username
    length username `shouldSatisfy` (> 0)

  it "username is lowercase" $ do
    let username = runFakerSeed 200 Internet.username
    all (\c -> isLower c || c == '_' || c == '.') username `shouldBe` True

  it "email contains @" $ do
    let email = runFakerSeed 300 Internet.email
    email `shouldContain` "@"

  it "safeEmail has valid domain" $ do
    let email = runFakerSeed 400 Internet.safeEmail
    email `shouldContain` "@"
    email `shouldContain` "."

  it "freeEmail uses free domains" $ do
    let email = runFakerSeed 500 Internet.freeEmail
    let freeDomains = ["gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "aol.com", "mail.com"]
    any (\domain -> domain `isInfixOf` email) freeDomains `shouldBe` True

  it "domain is non-empty" $ do
    let domain = runFakerSeed 600 Internet.domain
    length domain `shouldSatisfy` (> 0)

  it "domain contains dot" $ do
    let domain = runFakerSeed 700 Internet.domain
    domain `shouldContain` "."

  it "domainSuffix is valid" $ do
    let suffix = runFakerSeed 800 Internet.domainSuffix
    suffix `shouldSatisfy` (`elem` ["com", "org", "net", "edu", "gov", "co.uk"])

  it "url starts with https" $ do
    let url = runFakerSeed 900 Internet.url
    url `shouldStartWith` "https://"

  it "slug is non-empty" $ do
    let slug = runFakerSeed 1000 Internet.slug
    length slug `shouldSatisfy` (> 0)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tails :: String -> [String]
tails [] = [[]]
tails s@(_:xs) = s : tails xs
