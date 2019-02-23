{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Deriving.Enum (NamedEnum(..), nameToEnumDefault, GNamedEnum, WrapEnum(..)) where

import Data.Aeson
import Data.Coerce
import Data.Proxy
import Data.String
import Data.Text as T
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits
import qualified Data.HashMap.Strict as HM

class Enum a => NamedEnum a where
  enumToName :: IsString s => a -> s
  nameToEnum :: Text -> Maybe a

  default enumToName :: (Generic a, GNamedEnum (Rep a), IsString s) => a -> s
  enumToName = toConName . from

  default nameToEnum :: (Generic a, GNamedEnum (Rep a)) => Text -> Maybe a
  nameToEnum = nameToEnumDefault

nameToEnumDefault :: forall a s. (Enum a, Generic a, GNamedEnum (Rep a)) => Text -> Maybe a
nameToEnumDefault = \t -> toEnum <$> HM.lookup t m where
  (_, m) = genConNames @ (Rep a) 0 HM.empty

class GNamedEnum f where
  toConName :: IsString s => f x -> s
  genConNames :: forall f. Int -> HM.HashMap Text Int -> (Int, HM.HashMap Text Int)

instance GNamedEnum f => GNamedEnum (D1 meta f) where
  toConName (M1 f) = toConName f
  genConNames = genConNames @ f

instance KnownSymbol name => GNamedEnum (C1 ('MetaCons name fixity rec) U1) where
  toConName _ = fromString $ symbolVal (Proxy @ name)
  genConNames i m = (i + 1, HM.insert (fromString $ symbolVal (Proxy @ name)) i m)

instance (GNamedEnum f, GNamedEnum g) => GNamedEnum (f :+: g) where
  toConName (L1 x) = toConName x
  toConName (R1 x) = toConName x
  genConNames i m = case genConNames @ f i m of
    (j, m') -> genConNames @ g j m'

newtype WrapEnum a = WrapEnum { unwrapEnum :: a } deriving Enum

instance (Generic a, Enum a, GNamedEnum (Rep a)) => NamedEnum (WrapEnum a) where
  enumToName = toConName . from . unwrapEnum
  nameToEnum = coerce (nameToEnumDefault @ a)

instance NamedEnum a => FromJSON (WrapEnum a) where
  parseJSON = withText "WrapEnum"
    $ maybe (fail "Constructor not found") (pure . WrapEnum) . nameToEnum

instance NamedEnum a => ToJSON (WrapEnum a) where
  toJSON = coerce (enumToName @ a @Value)
