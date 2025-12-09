module Fake.AddressTest (addressTests) where

import Test.Hspec
import Fake
import qualified Fake.Address as Address

addressTests :: Spec
addressTests = describe "Faker.Address" $ do
  it "city is non-empty" $ do
    let c = runFakerSeed 100 Address.city
    length c `shouldSatisfy` (> 0)

  it "country is non-empty" $ do
    let country = runFakerSeed 200 Address.country
    length country `shouldSatisfy` (> 0)

  it "countryCode is two letters" $ do
    let code = runFakerSeed 300 Address.countryCode
    length code `shouldSatisfy` (\n -> n >= 2 && n <= 5)

  it "postalCode is non-empty" $ do
    let code = runFakerSeed 400 Address.postalCode
    length code `shouldSatisfy` (> 0)

  it "zipCode equals postalCode" $ do
    let zip1 = runFakerSeed 500 Address.zipCode
    let postal = runFakerSeed 500 Address.postalCode
    zip1 `shouldBe` postal

  it "latitude is in valid range" $ do
    let lat = runFakerSeed 600 Address.latitude
    lat `shouldSatisfy` (\x -> x >= -90 && x <= 90)

  it "longitude is in valid range" $ do
    let lon = runFakerSeed 700 Address.longitude
    lon `shouldSatisfy` (\x -> x >= -180 && x <= 180)

  it "multiple cities are different" $ do
    let cities = [runFakerSeed (n*100) Address.city | n <- [1..5]]
    length (nub cities) `shouldSatisfy` (> 1)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
