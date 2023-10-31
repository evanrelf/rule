{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Rule.Env where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import Rule.HList (HList (..))

data FieldName :: Symbol -> Type where
  FieldName :: KnownSymbol s => Proxy s -> FieldName s

instance (l ~ s, KnownSymbol s) => IsLabel l (FieldName s) where
  fromLabel = FieldName (Proxy @s)

data Field k v = Field (FieldName k) v

type (:::) = Field

newtype Env' xs a = Env (HList xs)

type Env xs = Env' xs ()

instance HasField k (Env '[Field k v]) v where
  getField :: Env '[Field k v] -> v
  getField (Env (Field _ v `HCons` HNil)) = v

instance HasField k (Env xs) v => HasField k (Env (x : xs)) v where
  getField (Env (_ `HCons` xs)) = getField @k (Env xs :: Env xs)

empty :: Env '[]
empty = Env HNil

insert :: FieldName k -> v -> Env xs -> Env (Field k v : xs)
insert k v (Env xs) = Env (Field k v `HCons` xs)
