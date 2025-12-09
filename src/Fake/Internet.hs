module Fake.Internet
  ( email
  , safeEmail
  , freeEmail
  , username
  , domain
  , domainSuffix
  , url
  , slug
  ) where

import Fake.Core (Fake)
import Fake.Primitives (elements, integerRange, string)
import Fake.Combinators (oneof, vectorOf)
import Fake.Person (firstName, lastName)
import Data.Char (toLower)

email :: Fake String
email = oneof [safeEmail, freeEmail]

safeEmail :: Fake String
safeEmail = do
  user <- username
  dom <- domain
  return $ user ++ "@" ++ dom

freeEmail :: Fake String
freeEmail = do
  user <- username
  provider <- elements freeDomains
  return $ user ++ "@" ++ provider

username :: Fake String
username = do
  first <- firstName
  lname <- lastName
  sep <- elements [".", "_", ""]
  let combined = map toLower first ++ sep ++ map toLower lname
  return combined

domain :: Fake String
domain = do
  name <- firstName
  suffix <- domainSuffix
  return $ map toLower name ++ "." ++ suffix

domainSuffix :: Fake String
domainSuffix = elements ["com", "org", "net", "edu", "gov", "co.uk"]

url :: Fake String
url = do
  dom <- domain
  path <- slug
  return $ "https://" ++ dom ++ "/" ++ path

slug :: Fake String
slug = do
   wordList <- vectorOf (1, 3) wordSlug
   return $ unwords wordList
  where
    wordSlug = do
      len <- integerRange 3 10
      word <- string len
      return $ map toLower word

freeDomains :: [String]
freeDomains =
  [ "gmail.com", "yahoo.com", "hotmail.com"
  , "outlook.com", "aol.com", "mail.com"
  ]
