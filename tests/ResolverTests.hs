{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module ResolverTests (tests) where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import Data.Aeson (encode)
import GraphQL
  ( Response(..)
  , interpretAnonymousQuery
  )
import GraphQL.API
  ( Object
  , Field
  , Argument
  , Enum
  , (:>)
  )
import GraphQL.Resolver
  ( Handler
  , ResolverError(..)
  , (:<>)(..)
  , AsGraphQLError(..)
  )
import GraphQL.Internal.Output (singleError)

import EnumTests ( Mode(NormalFile) )

import Control.Monad.Except (MonadError, throwError)

-- Test a custom error monad
type TMonad = ExceptT CustomError IO
type T = Object "T" '[] '[ Field "z" Int32
                         , Argument "x" Int32 :> Field "t" Int32
                         , Argument "y" Int32 :> Field "q" Int32
                         ]

tHandler :: Handler TMonad T
tHandler =
  pure $ pure 10 :<> pure :<> (pure . (*2))

-- Allow for throwError with custom error within a handler
newtype CustomError
  = UnknownUserID Text

instance AsGraphQLError CustomError where
  asGraphQLError (UnknownUserID _) = HandlerError "Unknown UserID" 404

type EM = Object "EM" '[]
  '[ Argument "id" Text :> Field "userName" Text
   ]

emHandler :: (MonadError CustomError m) => Handler m EM
emHandler =
  pure
    (\id -> case id of
                "1" -> pure "bob"
                _   -> throwError $ UnknownUserID id
    )

-- https://github.com/jml/graphql-api/issues/119
-- Maybe X didn't descend into its argument. Now it does.
type Query = Object "Query" '[]
  '[ Argument "id" Text :> Field "test" (Maybe Foo) ]

type Foo = Object "Foo" '[]
  '[ Field "name" Text ]

newtype ServerFoo = ServerFoo
  { name :: Text
  } deriving (Eq, Show)

lookupFoo :: (MonadError e m, AsGraphQLError e) => Text -> m (Maybe ServerFoo)
lookupFoo _ = pure $ Just (ServerFoo "Mort")

viewFoo :: (MonadError e m, AsGraphQLError e) => ServerFoo -> Handler m Foo
viewFoo ServerFoo { name=name } = pure $ pure name

handler :: (MonadError CustomError m) => Handler m Query
handler = pure $ \fooId -> do
  foo <- lookupFoo fooId
  -- note that fmap maps over the Maybe, so we still need
  -- have to wrap the result in a pure.
  traverse (pure . viewFoo) foo

-- Enum test
type EnumQuery = Object "File" '[]
  '[ Field "mode" (Enum "modeEnumName" Mode) ]

enumHandler :: (MonadError CustomError m) => Handler m EnumQuery
enumHandler = pure $ pure NormalFile
-- /Enum test

tests :: (MonadError e m, MonadIO m) => m TestTree
tests = liftIO $ testSpec "TypeAPI" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      Right (Success object) <- runExceptT
        (interpretAnonymousQuery @T tHandler "{ t(x: 12) }")
      encode object `shouldBe` "{\"t\":12}"
    it "complains about missing field" $ do
      Right (PartialSuccess _ errs) <- runExceptT
        (interpretAnonymousQuery @T tHandler "{ not_a_field }")
      errs `shouldBe` singleError (FieldNotFoundError "not_a_field")
    it "complains about missing argument" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t }")
      errs `shouldBe` singleError (ValueMissing "x")

  describe "issue 119" $ do
    it "Just works" $ do
      Right (Success object) <- runExceptT
        (interpretAnonymousQuery @Query handler "{ test(id: \"10\") { name } }")
      encode object `shouldBe` "{\"test\":{\"name\":\"Mort\"}}"
  describe "Parse, validate and execute queries against API" $ do
    it "API.Enum works" $ do
      Right (Success object) <- runExceptT
        (interpretAnonymousQuery @EnumQuery enumHandler "{ mode }")
      encode object `shouldBe` "{\"mode\":\"NormalFile\"}"

  describe "emTests" $ do
    it "finds a user" $ do
      Right (Success object) <- runExceptT
        (interpretAnonymousQuery @EM emHandler "{ userName(id: \"1\") }")
      encode object `shouldBe` "{\"userName\":\"bob\"}"
    it "adds an error otherwise" $ do
      Right (PartialSuccess _ errs) <- runExceptT
        (interpretAnonymousQuery @EM emHandler "{ userName(id: \"gibberish\") }")
      errs `shouldBe` singleError (HandlerError "Unknown UserID" 404)
