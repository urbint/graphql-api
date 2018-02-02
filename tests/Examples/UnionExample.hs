{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Examples.UnionExample  where

import Protolude
import GraphQL.API (Field, List, Object, Union)
import GraphQL (Response, interpretAnonymousQuery)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue, AsGraphQLError(..), ResolverError(..))

-- Slightly reduced example from the spec
type MiniCat = Object "MiniCat" '[] '[Field "name" Text, Field "meowVolume" Int32]
type MiniDog = Object "MiniDog" '[] '[Field "barkVolume" Int32]

type CatOrDog = Object "Me" '[]
  '[ Field "myPet" (Union "CatOrDog" '[MiniCat, MiniDog])
   ]
type CatOrDogList = Object "CatOrDogList" '[]
  '[ Field "pets" (List (Union "CatOrDog" '[MiniCat, MiniDog]))
   ]

newtype CustomError
  = UnknownUserID Text

instance AsGraphQLError CustomError where
  asGraphQLError (UnknownUserID _) = HandlerError "Unknown UserID" 404

miniCat :: (AsGraphQLError e, MonadError e m) => Text -> Handler m MiniCat
miniCat name = pure (pure name :<> pure 32)

miniDog :: (AsGraphQLError e, MonadError e m) => Handler m MiniDog
miniDog = pure (pure 100)

catOrDog :: (MonadError CustomError m) => Handler m CatOrDog
catOrDog = pure $ do
  name <- pure "MonadicFelix" -- we can do monadic actions
  unionValue @MiniCat (miniCat name)

catOrDogList :: (MonadError CustomError m) => Handler m CatOrDogList
catOrDogList = pure $
  pure [ unionValue @MiniCat (miniCat "Felix")
       , unionValue @MiniCat (miniCat "Mini")
       , unionValue @MiniDog miniDog
       ]

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import GraphQL.Value.ToValue (ToValue(..))

-- | Show usage of a single unionValue
--
-- >>> response <- exampleQuery
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"myPet":{"meowVolume":32,"name":"MonadicFelix"}}}
exampleQuery :: Monad m => m Response
exampleQuery = do
  Right res <- runExceptT $ interpretAnonymousQuery @CatOrDog catOrDog
    "{ myPet { ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } } }"

  pure res

-- | 'unionValue' can be used in a list context
--
-- >>> response <- exampleListQuery
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"pets":[{"meowVolume":32,"name":"Felix"},{"meowVolume":32,"name":"Mini"},{"barkVolume":100}]}}
exampleListQuery :: Monad m => m Response
exampleListQuery = do
  Right res <- runExceptT $ interpretAnonymousQuery @CatOrDogList catOrDogList
    "{ pets { ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } } }"

  pure res
