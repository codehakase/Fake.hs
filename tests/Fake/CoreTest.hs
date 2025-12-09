module Fake.CoreTest (coreTests) where

import Test.Hspec
import Fake
import qualified Fake.Person as Person
import qualified Fake.Internet as Internet

coreTests :: Spec
coreTests = describe "Faker.Core" $ do
  it "runFakerSeed is deterministic" $ do
    runFakerSeed 42 Person.fullName `shouldBe` runFakerSeed 42 Person.fullName

  it "generates non-empty names" $ do
    let name = runFakerSeed 123 Person.fullName
    length name `shouldSatisfy` (> 0)

  it "generates valid email addresses" $ do
    let email = runFakerSeed 456 Internet.email
    email `shouldContain` "@"
    email `shouldContain` "."

  it "email contains domain" $ do
    let email = runFakerSeed 789 Internet.email
    words email `shouldNotBe` []
