module Fake.Internet (
    email,
    safeEmail,
    freeEmail,
    username,
    domain,
    domainSuffix,
    url,
    slug,
) where

import Data.Char (toLower)
import Fake.Combinators (oneof, vectorOf)
import Fake.Core (Fake)
import Fake.Person (firstName, lastName)
import Fake.Primitives (elements, integerRange, string)

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
    return $ map toLower first ++ sep ++ map toLower lname

domain :: Fake String
domain = (\n suffix -> map toLower n ++ "." ++ suffix) <$> firstName <*> domainSuffix

domainSuffix :: Fake String
domainSuffix = elements ["com", "org", "net", "edu", "gov", "co.uk"]

url :: Fake String
url = do
    dom <- domain
    path <- slug
    return $ "https://" ++ dom ++ "/" ++ path

slug :: Fake String
slug = unwords <$> vectorOf (1, 3) wordSlug
  where
    wordSlug = do
        len <- integerRange 3 10
        map toLower <$> string len

freeDomains :: [String]
freeDomains =
    [ "gmail.com"
    , "yahoo.com"
    , "hotmail.com"
    , "outlook.com"
    , "aol.com"
    , "mail.com"
    ]
