module Fake.Address (
    city,
    country,
    countryCode,
    postalCode,
    zipCode,
    latitude,
    longitude,
) where

import Control.Monad (replicateM)
import Fake.Core (Fake)
import Fake.Primitives (double, elements, numeric)

city :: Fake String
city = elements cities

country :: Fake String
country = elements countries

countryCode :: Fake String
countryCode = elements countryCodes

postalCode :: Fake String
postalCode = replicateM 5 numeric

zipCode :: Fake String
zipCode = postalCode

latitude :: Fake Double
latitude = subtract 90 . (* 180) <$> double

longitude :: Fake Double
longitude = subtract 180 . (* 360) <$> double

cities :: [String]
cities =
    [ "New York"
    , "Los Angeles"
    , "Chicago"
    , "Houston"
    , "Phoenix"
    , "Philadelphia"
    , "San Antonio"
    , "San Diego"
    , "Dallas"
    , "San Jose"
    , "Austin"
    , "Jacksonville"
    , "Fort Worth"
    , "Columbus"
    , "Boston"
    , "Denver"
    , "Washington"
    , "Nashville"
    , "Seattle"
    , "Portland"
    , "Lagos"
    , "Cairo"
    , "Johannesburg"
    , "Nairobi"
    , "Accra"
    , "Addis Ababa"
    , "Casablanca"
    , "Tunis"
    , "Kampala"
    , "Harare"
    , "Gaborone"
    , "Windhoek"
    , "Dakar"
    , "Abidjan"
    , "Yaoundé"
    ]

countries :: [String]
countries =
    [ "United States"
    , "Canada"
    , "Mexico"
    , "United Kingdom"
    , "France"
    , "Germany"
    , "Italy"
    , "Spain"
    , "Japan"
    , "China"
    , "India"
    , "Brazil"
    , "Russia"
    , "Australia"
    , "South Korea"
    , "Netherlands"
    , "Sweden"
    , "Switzerland"
    , "Norway"
    , "Denmark"
    , "South Africa"
    , "Nigeria"
    , "Egypt"
    , "Kenya"
    , "Ghana"
    , "Ethiopia"
    , "Morocco"
    , "Tunisia"
    , "Uganda"
    , "Zimbabwe"
    , "Botswana"
    , "Namibia"
    , "Senegal"
    , "Ivory Coast"
    , "Cameroon"
    ]

countryCodes :: [String]
countryCodes =
    [ "US"
    , "CA"
    , "MX"
    , "GB"
    , "FR"
    , "DE"
    , "IT"
    , "ES"
    , "JP"
    , "CN"
    , "IN"
    , "BR"
    , "RU"
    , "AU"
    , "KR"
    , "NL"
    , "SE"
    , "CH"
    , "NO"
    , "DK"
    , "ZA"
    , "NG"
    , "EG"
    , "KE"
    , "GH"
    , "ET"
    , "MA"
    , "TN"
    , "UG"
    , "ZW"
    , "BW"
    , "NA"
    , "SN"
    , "CI"
    , "CM"
    ]
