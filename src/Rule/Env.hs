{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Rule.Env
  ( Env
  , (:::)
  , empty
  , insert
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

infixr 5 `HCons`

data FieldName :: Symbol -> Type where
  FieldName :: KnownSymbol s => Proxy s -> FieldName s

instance (l ~ s, KnownSymbol s) => IsLabel l (FieldName s) where
  fromLabel = FieldName (Proxy @s)

data k ::: v = FieldName k := v

infix 6 :::, :=

newtype Env xs = Env (HList xs)

instance HasField k (Env '[k ::: v]) v where
  getField :: Env '[k ::: v] -> v
  getField (Env (_ := v `HCons` HNil)) = v

instance HasField k (Env xs) v => HasField k (Env (x : xs)) v where
  getField (Env (_ `HCons` xs)) = getField @k (Env xs)

empty :: Env '[]
empty = Env HNil

insert :: FieldName k -> v -> Env xs -> Env (k ::: v : xs)
insert k v (Env xs) = Env (k := v `HCons` xs)
