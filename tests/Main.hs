module Main where

import Test.Hspec
import Fake.CoreTest
import Fake.PrimitivesTest
import Fake.InternetTest
import Fake.AddressTest

main :: IO ()
main = hspec $ do
  coreTests
  primitivesTests
  internetTests
  addressTests
