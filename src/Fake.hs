module Fake
  ( Fake
  , runFaker
  , runFakerSeed
  , runFakerWith
  , integer
  , integerRange
  , natural
  , float
  , double
  , bool
  , char
  , alpha
  , numeric
  , alphanumeric
  , string
  , stringBounded
  , elements
  , shuffle
  , vector
  , vectorOf
  , Fake.Combinators.sequence
  , maybeGen
  , oneof
  , filterGen
  , mapGen
  , flatMap
  ) where

import Fake.Core (Fake, runFaker, runFakerSeed, runFakerWith)
import Fake.Primitives
  ( integer
  , integerRange
  , natural
  , float
  , double
  , bool
  , char
  , alpha
  , numeric
  , alphanumeric
  , string
  , stringBounded
  , elements
  , shuffle
  )
import Fake.Combinators
  ( vector
  , vectorOf
  , maybeGen
  , oneof
  , filterGen
  , mapGen
  , flatMap
  )
import qualified Fake.Combinators
