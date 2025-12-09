# Fake

A pure, composable Haskell faker library for generating fake data in tests. Built with the State monad for deterministic, reproducible test data generation.

## Features

- **Pure Functions**: No global state or I/O effects in generators
- **Deterministic**: Seeded generation for reproducible tests
- **Composable**: Monadic combinators for building complex generators
- **Type Safe**: Concrete return types, no stringly-typed APIs
- **Minimal Dependencies**: Only standard Haskell libraries
- **Easy to Use**: Simple, intuitive API

## Installation

```bash
cabal build
cabal test
```

## Usage

### Basic Generation

```haskell
import Fake
import qualified Fake.Person as Person
import qualified Fake.Internet as Internet

-- Deterministic generation
let name = runFakerSeed 42 Person.fullName
let email = runFakerSeed 42 Internet.email

-- Non-deterministic generation
randomName <- runFaker Person.fullName
```

### Complex Objects

```haskell
data User = User
  { name :: String
  , email :: String
  , age :: Int
  } deriving (Show)

generateUser :: Fake User
generateUser = do
  n <- Person.fullName
  e <- Internet.email
  a <- integerRange 18 80
  return $ User n e a

user <- runFaker generateUser
```

### Using Combinators

```haskell
-- Generate 5-10 emails
emails <- runFaker $ vectorOf (5, 10) Internet.email

-- Optional value (50% chance)
username <- runFaker $ maybeGen Internet.username

-- Pick one from list
email <- runFaker $ oneof [Internet.safeEmail, Internet.freeEmail]
```

## Architecture

The library uses a State monad wrapping `StdGen` for pure, composable random generation:

```haskell
newtype Fake a = Fake (State StdGen a)
  deriving (Functor, Applicative, Monad)
```

Three execution modes:
- `runFaker :: Fake a -> IO a` - System random
- `runFakerSeed :: Int -> Fake a -> a` - Deterministic
- `runFakerWith :: StdGen -> Fake a -> a` - Custom RNG


## Available Generators

### Primitives
- Numbers: `integer`, `integerRange`, `natural`, `float`, `double`, `bool`
- Characters: `char`, `alpha`, `numeric`, `alphanumeric`
- Strings: `string`, `stringBounded`
- Selection: `elements`, `shuffle`

### Combinators
- Repetition: `vector`, `vectorOf`, `sequence`
- Optionality: `maybeGen`, `oneof`
- Functional: `filterGen`, `mapGen`, `flatMap`

### Person
- Names: `firstName`, `lastName`, `fullName`, `name`
- Titles: `title`, `jobTitle`, `prefix`, `suffix`
- Bio: `bio`

### Internet
- Email: `email`, `safeEmail`, `freeEmail`
- Usernames: `username`, `domain`, `domainSuffix`
- URLs: `url`, `slug`

### Address
- Geography: `city`, `country`, `countryCode`
- Postal: `postalCode`, `zipCode`
- Coordinates: `latitude`, `longitude`

## Testing

```haskell
import Test.Hspec
import Fake
import qualified Fake.Person as Person

spec :: Spec
spec = describe "User generation" $ do
  it "generates consistent names with same seed" $ do
    let name1 = runFakerSeed 123 Person.fullName
    let name2 = runFakerSeed 123 Person.fullName
    name1 `shouldBe` name2
```

Run tests:
```bash
cabal test
```

