{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Rule.Core
  ( FieldName (..)
  , (:::) (..)
  , HList (..)
  , Rule (..)
  , runRule
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)

data FieldName :: Symbol -> Type where
  FieldName :: KnownSymbol s => Proxy s -> FieldName s

instance (l ~ s, KnownSymbol s) => IsLabel l (FieldName s) where
  fromLabel = FieldName (Proxy @s)

data k ::: v = FieldName k := v

infix 6 :::, :=

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

infixr 5 `HCons`

instance HasField k (HList '[k ::: v]) v where
  getField :: HList '[k ::: v] -> v
  getField (_ := v `HCons` HNil) = v

instance HasField k (HList xs) v => HasField k (HList (x : xs)) v where
  getField (_ `HCons` xs) = getField @k xs

newtype Rule env a = Rule (HList env -> a)

runRule :: HList env -> Rule env a -> a
runRule env (Rule f) = f env
