{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level definitions for a GraphQL schema.
module GraphQL.API
  ( Object
  , Field
  , Argument
  , DefaultArgument
  , Union
  , List
  , Enum
  , GraphQLEnum(..)
  , Interface
  , (:>)(..)
  , HasAnnotatedType(..)
  , HasAnnotatedInputType
  , HasObjectDefinition(..)
  , getArgumentDefinition
  -- | Exported for testing. Perhaps should be a different module.
  , getFieldDefinition
  , getInterfaceDefinition
  , getAnnotatedInputType
  ) where

import Protolude hiding (Enum, TypeError)

import GraphQL.Internal.Schema hiding (Type)
import qualified GraphQL.Internal.Schema (Type)
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import GraphQL.Internal.Name (NameError, nameFromSymbol)
import GraphQL.API.Enum (GraphQLEnum(..))
import GHC.Generics ((:*:)(..))
import GHC.Types (Type)


-- $setup
-- >>> :set -XDataKinds -XTypeOperators

-- | Argument operator. Can only be used with 'Field'.
--
-- Say we have a @Company@ object that has a field that shows whether
-- someone is an employee, e.g.
--
-- @
--   type Company {
--     hasEmployee(employeeName: String!): String!
--   }
-- @
--
-- Then we might represent that as:
--
-- >>> type Company = Object "Company" '[] '[Argument "employeeName" Text :> Field "hasEmployee" Bool]
--
-- For multiple arguments, simply chain them together with ':>', ending
-- finally with 'Field'. e.g.
--
-- @
--   Argument "foo" String :> Argument "bar" Int :> Field "qux" Int
-- @
data a :> b = a :> b
infixr 8 :>


data Object (name :: Symbol) (interfaces :: [Type]) (fields :: [Type])
data Enum (name :: Symbol) (values :: Type)
data Union (name :: Symbol) (types :: [Type])
data List (elemType :: Type)

-- TODO(tom): AFACIT We can't constrain "fields" to e.g. have at least
-- one field in it - is this a problem?
data Interface (name :: Symbol) (fields :: [Type])
data Field (name :: Symbol) (fieldType :: Type)
data Argument (name :: Symbol) (argType :: Type)


-- Can't set the value for default arguments via types, but can
-- distinguish to force users to provide a default argument somewhere
-- in their function (using Maybe? ore some new type like
-- https://hackage.haskell.org/package/optional-args-1.0.1)
data DefaultArgument (name :: Symbol) (argType :: Type)

cons :: a -> [a] -> [a]
cons = (:)

-- Transform into a Schema definition
class HasObjectDefinition a where
  -- Todo rename to getObjectTypeDefinition
  getDefinition :: Either NameError ObjectTypeDefinition

class HasFieldDefinition a where
  getFieldDefinition :: Either NameError FieldDefinition


-- Fields
class HasFieldDefinitions a where
  getFieldDefinitions :: Either NameError [FieldDefinition]

instance forall a as. (HasFieldDefinition a, HasFieldDefinitions as) => HasFieldDefinitions (a:as) where
  getFieldDefinitions = cons <$> getFieldDefinition @a <*> getFieldDefinitions @as

instance HasFieldDefinitions '[] where
  getFieldDefinitions = pure []


-- object types from union type lists, e.g. for
-- Union "Horse" '[Leg, Head, Tail]
--               ^^^^^^^^^^^^^^^^^^ this part
class UnionTypeObjectTypeDefinitionList a where
  getUnionTypeObjectTypeDefinitions :: Either NameError [ObjectTypeDefinition]

instance forall a as. (HasObjectDefinition a, UnionTypeObjectTypeDefinitionList as) => UnionTypeObjectTypeDefinitionList (a:as) where
  getUnionTypeObjectTypeDefinitions = cons <$> getDefinition @a <*> getUnionTypeObjectTypeDefinitions @as

instance UnionTypeObjectTypeDefinitionList '[] where
  getUnionTypeObjectTypeDefinitions = pure []

-- Interfaces
class HasInterfaceDefinitions a where
  getInterfaceDefinitions :: Either NameError Interfaces

instance forall a as. (HasInterfaceDefinition a, HasInterfaceDefinitions as) => HasInterfaceDefinitions (a:as) where
  getInterfaceDefinitions = cons <$> getInterfaceDefinition @a <*> getInterfaceDefinitions @as

instance HasInterfaceDefinitions '[] where
  getInterfaceDefinitions = pure []

class HasInterfaceDefinition a where
  getInterfaceDefinition :: Either NameError InterfaceTypeDefinition

instance forall ks fields. (KnownSymbol ks, HasFieldDefinitions fields) => HasInterfaceDefinition (Interface ks fields) where
  getInterfaceDefinition =
    let name = nameFromSymbol @ks
        fields = NonEmptyList <$> getFieldDefinitions @fields
    in InterfaceTypeDefinition <$> name <*> fields

-- Give users some help if they don't terminate Arguments with a Field:
-- NB the "redundant constraints" warning is a GHC bug: https://ghc.haskell.org/trac/ghc/ticket/11099
instance forall ks t. TypeError ('Text ":> Arguments must end with a Field") =>
         HasFieldDefinition (Argument ks t) where
  getFieldDefinition = notImplemented

instance forall ks is ts. (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions ts) => HasAnnotatedType (Object ks is ts) where
  getAnnotatedType =
    let obj = getDefinition @(Object ks is ts)
    in (TypeNamed . DefinedType . TypeDefinitionObject) <$> obj

instance forall t ks. (KnownSymbol ks, HasAnnotatedType t) => HasFieldDefinition (Field ks t) where
  getFieldDefinition =
    let name = nameFromSymbol @ks
    in FieldDefinition <$> name <*> pure [] <*> getAnnotatedType @t

class HasArgumentDefinition a where
  getArgumentDefinition :: Either NameError ArgumentDefinition

instance forall ks t. (KnownSymbol ks, HasAnnotatedInputType t) => HasArgumentDefinition (Argument ks t) where
  getArgumentDefinition = ArgumentDefinition <$> argName <*> argType <*> defaultValue
    where
      argName = nameFromSymbol @ks
      argType = getAnnotatedInputType @t
      defaultValue = pure Nothing

instance forall a b. (HasArgumentDefinition a, HasFieldDefinition b) => HasFieldDefinition (a :> b) where
  getFieldDefinition =
    prependArg <$> argument <*> getFieldDefinition @b
    where
      prependArg arg (FieldDefinition name argDefs at) = FieldDefinition name (arg:argDefs) at
      argument = getArgumentDefinition @a

instance forall ks is fields.
  (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions fields) =>
  HasObjectDefinition (Object ks is fields) where
  getDefinition =
    let name = nameFromSymbol @ks
        interfaces = getInterfaceDefinitions @is
        fields = NonEmptyList <$> getFieldDefinitions @fields
    in ObjectTypeDefinition <$> name <*> interfaces <*> fields

-- Builtin output types (annotated types)
class HasAnnotatedType a where
  -- TODO - the fact that we have to return TypeNonNull for normal
  -- types will amost certainly lead to bugs because people will
  -- forget this. Maybe we can flip the internal encoding to be
  -- non-null by default and needing explicit null-encoding (via
  -- Maybe).
  getAnnotatedType :: Either NameError (AnnotatedType GraphQL.Internal.Schema.Type)

-- | Turn a non-null type into the optional version of its own type.
dropNonNull :: AnnotatedType t -> AnnotatedType t
dropNonNull (TypeNonNull (NonNullTypeNamed t)) = TypeNamed t
dropNonNull (TypeNonNull (NonNullTypeList t)) = TypeList t
dropNonNull x@(TypeNamed _) = x
dropNonNull x@(TypeList _) = x

instance forall a. HasAnnotatedType a => HasAnnotatedType (Maybe a) where
  -- see TODO in HasAnnotatedType class
  getAnnotatedType = dropNonNull <$> getAnnotatedType @a

builtinType :: Builtin -> Either NameError (AnnotatedType GraphQL.Internal.Schema.Type)
builtinType = pure . TypeNonNull . NonNullTypeNamed . BuiltinType

-- TODO(jml): Given that AnnotatedType is parametrised, we can probably reduce
-- a great deal of duplication by making HasAnnotatedType a parametrised type
-- class.

-- TODO(jml): Be smarter and figure out how to say "all integral types" rather
-- than listing each individually.

instance HasAnnotatedType Int where
  getAnnotatedType = builtinType GInt

instance HasAnnotatedType Int32 where
  getAnnotatedType = builtinType GInt

instance HasAnnotatedType Bool where
  getAnnotatedType = builtinType GBool

instance HasAnnotatedType Text where
  getAnnotatedType = builtinType GString

instance HasAnnotatedType Double where
  getAnnotatedType = builtinType GFloat

instance HasAnnotatedType Float where
  getAnnotatedType = builtinType GFloat

instance forall t. (HasAnnotatedType t) => HasAnnotatedType (List t) where
  getAnnotatedType = TypeList . ListType <$> getAnnotatedType @t

instance forall ks enum. (KnownSymbol ks, GraphQLEnum enum) => HasAnnotatedType (Enum ks enum) where
  getAnnotatedType = do
    let name = nameFromSymbol @ks
    let enums = sequenceA (enumValues @enum) :: Either NameError [Name]
    let et = EnumTypeDefinition <$> name <*> map (map EnumValueDefinition) enums
    TypeNonNull . NonNullTypeNamed . DefinedType . TypeDefinitionEnum <$> et

instance forall ks as. (KnownSymbol ks, UnionTypeObjectTypeDefinitionList as) => HasAnnotatedType (Union ks as) where
  getAnnotatedType =
    let name = nameFromSymbol @ks
        types = NonEmptyList <$> getUnionTypeObjectTypeDefinitions @as
    in (TypeNamed . DefinedType . TypeDefinitionUnion) <$> (UnionTypeDefinition <$> name <*> types)

-- Help users with better type errors
instance TypeError ('Text "Cannot encode Integer because it has arbitrary size but the JSON encoding is a number") =>
         HasAnnotatedType Integer where
  getAnnotatedType = undefined


-- Builtin input types
class HasAnnotatedInputType a where
  -- See TODO comment in "HasAnnotatedType" class for nullability.
  getAnnotatedInputType :: Either NameError (AnnotatedType InputType)
  default getAnnotatedInputType :: (Generic a, GenericAnnotatedInputType (Rep a)) => Either NameError (AnnotatedType InputType)
  getAnnotatedInputType = genericGetAnnotatedInputType @(Rep a)

instance forall a. HasAnnotatedInputType a => HasAnnotatedInputType (Maybe a) where
  getAnnotatedInputType = dropNonNull <$> getAnnotatedInputType @a

builtinInputType :: Builtin -> Either NameError (AnnotatedType InputType)
builtinInputType = pure . TypeNonNull . NonNullTypeNamed . BuiltinInputType

instance HasAnnotatedInputType Int where
  getAnnotatedInputType = builtinInputType GInt

instance HasAnnotatedInputType Int32 where
  getAnnotatedInputType = builtinInputType GInt

instance HasAnnotatedInputType Bool where
  getAnnotatedInputType = builtinInputType GBool

instance HasAnnotatedInputType Text where
  getAnnotatedInputType = builtinInputType GString

instance HasAnnotatedInputType Double where
  getAnnotatedInputType = builtinInputType GFloat

instance HasAnnotatedInputType Float where
  getAnnotatedInputType = builtinInputType GFloat

instance forall t. (HasAnnotatedInputType t) => HasAnnotatedInputType (List t) where
  getAnnotatedInputType = TypeList . ListType <$> getAnnotatedInputType @t

instance forall t. (HasAnnotatedInputType t) => HasAnnotatedInputType [t] where
  getAnnotatedInputType = TypeList . ListType <$> getAnnotatedInputType @t

instance forall ks enum. (KnownSymbol ks, GraphQLEnum enum) => HasAnnotatedInputType (Enum ks enum) where
  getAnnotatedInputType = do
    let name = nameFromSymbol @ks
        enums = sequenceA (enumValues @enum) :: Either NameError [Name]
    let et = EnumTypeDefinition <$> name <*> map (map EnumValueDefinition) enums
    TypeNonNull . NonNullTypeNamed . DefinedInputType . InputTypeDefinitionEnum <$> et


-- Generic getAnnotatedInputType function
class GenericAnnotatedInputType (f :: Type -> Type) where
  genericGetAnnotatedInputType :: Either NameError (AnnotatedType InputType)

class GenericInputObjectFieldDefinitions (f :: Type -> Type) where
  genericGetInputObjectFieldDefinitions :: Either NameError [InputObjectFieldDefinition]

instance forall dataName consName records s l p.
  ( KnownSymbol dataName
  , KnownSymbol consName
  , GenericInputObjectFieldDefinitions records
  ) => GenericAnnotatedInputType (D1 ('MetaData dataName s l 'False)
                                  (C1 ('MetaCons consName p 'True) records
                                  )) where
  genericGetAnnotatedInputType = do
    name <- nameFromSymbol @dataName
    map ( TypeNonNull
          . NonNullTypeNamed
          . DefinedInputType
          . InputTypeDefinitionObject
          . (InputObjectTypeDefinition name)
          . NonEmptyList
        ) (genericGetInputObjectFieldDefinitions @records)

instance forall wrappedType fieldName rest u s l.
  ( KnownSymbol fieldName
  , HasAnnotatedInputType wrappedType
  , GenericInputObjectFieldDefinitions rest
  ) => GenericInputObjectFieldDefinitions (S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType) :*: rest) where
  genericGetInputObjectFieldDefinitions = do
    name <- nameFromSymbol @fieldName
    annotatedInputType <- getAnnotatedInputType @wrappedType
    let l = InputObjectFieldDefinition name annotatedInputType Nothing
    r <- genericGetInputObjectFieldDefinitions @rest
    pure (l:r)

instance forall wrappedType fieldName u s l.
  ( KnownSymbol fieldName
  , HasAnnotatedInputType wrappedType
  ) => GenericInputObjectFieldDefinitions (S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType)) where
  genericGetInputObjectFieldDefinitions = do
    name <- nameFromSymbol @fieldName
    annotatedInputType <- getAnnotatedInputType @wrappedType
    let l = InputObjectFieldDefinition name annotatedInputType Nothing
    pure [l]
