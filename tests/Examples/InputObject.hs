{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.InputObject where
import Protolude hiding (Enum)

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler, Defaultable(..), ResolverError(..), AsGraphQLError(..))
import GraphQL.Value.FromValue (FromValue)

data DogStuff = DogStuff { toy :: Text, likesTreats :: Bool } deriving (Show, Generic)
instance FromValue DogStuff
instance HasAnnotatedInputType DogStuff
instance Defaultable DogStuff where
  -- TODO defaultFor takes a Name which makes sense, but what's the
  -- name for an input object?
  defaultFor _ = Just (DogStuff "shoe" False)

type Query = Object "Query" '[]
  '[ Argument "dogStuff" DogStuff :> Field "description" Text ]

newtype CustomError
  = UnknownUserID Text

instance AsGraphQLError CustomError where
  asGraphQLError (UnknownUserID _) = HandlerError "Unknown UserID" 404

root :: (MonadError CustomError m) => Handler m Query
root = pure description

description :: (AsGraphQLError e, MonadError e m) => DogStuff -> Handler m Text
description (DogStuff toy likesTreats)
  | likesTreats = pure $ "likes treats and their favorite toy is a " <> toy
  | otherwise = pure $ "their favorite toy is a " <> toy

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import GraphQL.Value.ToValue (ToValue(..))

-- | Show input object usage
--
-- >>> response <- example "{ description(dogStuff: {toy: \"bone\", likesTreats: true}) }"
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"description":"likes treats and their favorite toy is a bone"}}
--
-- >>> response <- example "{ description }"
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"description":"their favorite toy is a shoe"}}
example :: Monad m => Text -> m Response
example query = do
  Right res <- runExceptT $ interpretAnonymousQuery @Query root query

  pure res
