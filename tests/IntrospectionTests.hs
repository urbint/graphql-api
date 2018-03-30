{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

module IntrospectionTests (tests) where

import Protolude

import Data.Aeson (toJSON, object, (.=))
import GraphQL (interpretAnonymousQuery)
import GraphQL.API (Object, Field)
import GraphQL.Resolver((:<>)(..), Handler)
import GraphQL.Value.ToValue (ToValue(..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec
import Text.RawString.QQ (r)

import ExampleSchema

-- | Example query root.
--
-- @
-- type QueryRoot {
--   dog: Dog
-- }
-- @
--
-- Drawn from <https://facebook.github.io/graphql/#sec-Validation>.
type QueryRoot = Object "QueryRoot" '[]
  '[ Field "dog" Dog
   ]

-- | Our server's internal representation of a 'Dog'.
data ServerDog
  = ServerDog
    { name :: Text
    , nickname :: Maybe Text
    , barkVolume :: Int32
    , knownCommands :: Set DogCommand
    , houseTrainedAtHome :: Bool
    , houseTrainedElsewhere :: Bool
    , owner :: ServerHuman
    }

-- | Whether 'ServerDog' knows the given command.
doesKnowCommand :: ServerDog -> DogCommand -> Bool
doesKnowCommand dog command = command `elem` knownCommands dog

-- | Whether 'ServerDog' is house-trained.
isHouseTrained :: ServerDog -> Maybe Bool -> Bool
isHouseTrained dog Nothing = houseTrainedAtHome dog || houseTrainedElsewhere dog
isHouseTrained dog (Just False) = houseTrainedAtHome dog
isHouseTrained dog (Just True) = houseTrainedElsewhere dog

-- | Present 'ServerDog' for GraphQL.
viewServerDog :: ServerDog -> Handler IO Dog
viewServerDog dog@ServerDog{..} = pure $
  pure name :<>
  pure (fmap pure nickname) :<>
  pure barkVolume :<>
  pure . doesKnowCommand dog :<>
  pure . isHouseTrained dog :<>
  viewServerHuman owner

-- | jml has a stuffed black dog called "Mortgage".
mortgage :: ServerDog
mortgage = ServerDog
           { name = "Mortgage"
           , nickname = Just "Mort"
           , barkVolume = 0  -- He's stuffed
           , knownCommands = mempty  -- He's stuffed
           , houseTrainedAtHome = True  -- Never been a problem
           , houseTrainedElsewhere = True  -- Untested in the field
           , owner = jml
           }

-- | Our server's internal representation of a 'Human'.
newtype ServerHuman = ServerHuman Text deriving (Eq, Ord, Show)

-- | Present a 'ServerHuman' as a GraphQL 'Human'.
viewServerHuman :: ServerHuman -> Handler IO Human
viewServerHuman (ServerHuman name) = pure (pure name)

-- | It me.
jml :: ServerHuman
jml = ServerHuman "jml"

tests :: IO TestTree
tests = testSpec "Introspection" $ do
  it "supports __typename on regular objects" $ do
    let root = pure (viewServerDog mortgage)
        query = [r|{
                    dog {
                      __typename
                    }
                  }|]
        expected = object
          [ "data" .= object
            [ "dog" .= object
              [ "__typename" .= ("Dog" :: Text) ]]]

    response <- interpretAnonymousQuery @QueryRoot root query

    toJSON (toValue response) `shouldBe` expected

