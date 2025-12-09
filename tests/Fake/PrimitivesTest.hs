module Fake.PrimitivesTest (primitivesTests) where

import Test.Hspec
import Fake

primitivesTests :: Spec
primitivesTests = describe "Faker.Primitives" $ do
  it "integerRange stays within bounds" $ do
    let vals = [runFakerSeed (n*100) (integerRange 10 20) | n <- [1..10]]
    all (\v -> v >= 10 && v <= 20) vals `shouldBe` True

  it "natural generates non-negative integers" $ do
    let vals = [runFakerSeed (n*100) natural | n <- [1..10]]
    all (\v -> v >= 0) vals `shouldBe` True

  it "bool generates boolean values" $ do
    let val = runFakerSeed 999 bool
    (val == True || val == False) `shouldBe` True

  it "string generates correct length" $ do
    let str = runFakerSeed 111 (string 10)
    length str `shouldBe` 10

  it "stringBounded stays within bounds" $ do
    let str1 = runFakerSeed 111 (stringBounded (5, 15))
    let str2 = runFakerSeed 222 (stringBounded (5, 15))
    (length str1 >= 5 && length str1 <= 15) `shouldBe` True
    (length str2 >= 5 && length str2 <= 15) `shouldBe` True

  it "alpha generates alphabetic characters" $ do
    let c = runFakerSeed 444 alpha
    c `shouldSatisfy` (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

  it "numeric generates digit characters" $ do
    let c = runFakerSeed 555 numeric
    c `shouldSatisfy` (\x -> x >= '0' && x <= '9')

  it "elements picks from list" $ do
    let val = runFakerSeed 333 (elements [1, 2, 3, 4, 5 :: Int])
    val `shouldSatisfy` (`elem` [1, 2, 3, 4, 5])
