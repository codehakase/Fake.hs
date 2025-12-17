module Fake.Person (
    firstName,
    lastName,
    fullName,
    name,
    title,
    jobTitle,
    prefix,
    suffix,
    bio,
) where

import Fake.Core (Fake)
import Fake.Primitives (elements, integerRange, string)

firstName :: Fake String
firstName = elements names

lastName :: Fake String
lastName = elements lastNames

fullName :: Fake String
fullName = do
    first <- firstName
    lname <- lastName
    return $ first ++ " " ++ lname

name :: Fake String
name = fullName

title :: Fake String
title = elements jobTitles

jobTitle :: Fake String
jobTitle = title

prefix :: Fake String
prefix = elements prefixes

suffix :: Fake String
suffix = elements suffixes

bio :: Fake String
bio = do
    len <- integerRange 20 50
    string len

names :: [String]
names =
    [ "James"
    , "Robert"
    , "Michael"
    , "William"
    , "David"
    , "Richard"
    , "Joseph"
    , "Charles"
    , "Christopher"
    , "Donald"
    , "Matthew"
    , "Mark"
    , "Steven"
    , "Paul"
    , "Andrew"
    , "Joshua"
    , "Kenneth"
    , "Kevin"
    , "Brian"
    , "Edward"
    , "Ronald"
    , "Timothy"
    , "Jason"
    , "Jeffrey"
    , "Ryan"
    , "Mary"
    , "Patricia"
    , "Jennifer"
    , "Linda"
    , "Barbara"
    , "Elizabeth"
    , "Susan"
    , "Jessica"
    , "Sarah"
    , "Karen"
    , "Lisa"
    , "Nancy"
    , "Betty"
    , "Margaret"
    , "Sandra"
    , "Ashley"
    , "Kimberly"
    , "Emily"
    , "Donna"
    , "Michelle"
    , "Dorothy"
    , "Carol"
    , "Amanda"
    , "Melissa"
    , "Deborah"
    ]

lastNames :: [String]
lastNames =
    [ "Smith"
    , "Johnson"
    , "Williams"
    , "Brown"
    , "Jones"
    , "Garcia"
    , "Miller"
    , "Davis"
    , "Rodriguez"
    , "Martinez"
    , "Hernandez"
    , "Lopez"
    , "Gonzalez"
    , "Wilson"
    , "Anderson"
    , "Thomas"
    , "Taylor"
    , "Moore"
    , "Jackson"
    , "Martin"
    , "Lee"
    , "Perez"
    , "Thompson"
    , "White"
    , "Harris"
    ]

jobTitles :: [String]
jobTitles =
    [ "Software Engineer"
    , "Product Manager"
    , "Designer"
    , "Data Scientist"
    , "DevOps Engineer"
    , "Manager"
    , "Accountant"
    , "Lawyer"
    , "Doctor"
    , "Nurse"
    , "Teacher"
    , "Sales Representative"
    , "Consultant"
    , "Analyst"
    , "Director"
    , "VP"
    ]

prefixes :: [String]
prefixes = ["Mr.", "Mrs.", "Ms.", "Dr.", "Prof."]

suffixes :: [String]
suffixes = ["Jr.", "Sr.", "II", "III", "Ph.D.", "M.D."]
