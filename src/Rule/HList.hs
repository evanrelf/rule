{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rule.HList where

import Data.Kind (Constraint, Type)
import Prelude hiding (lookup)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

infixr 5 `HCons`

type Append :: [Type] -> [Type] -> [Type] -> Constraint
class Append xs ys zs | xs ys -> zs where
  append :: HList xs -> HList ys -> HList zs

instance Append '[] '[] '[] where
  append :: HList '[] -> HList '[] -> HList '[]
  append HNil HNil = HNil

instance Append (x : xs) '[] (x : xs) where
  append :: HList (x : xs) -> HList '[] -> HList (x : xs)
  append xs HNil = xs

instance Append '[] (y : ys) (y : ys) where
  append :: HList '[] -> HList (y : ys) -> HList (y : ys)
  append HNil ys = ys

instance Append xs (y : ys) zs => Append (x : xs) (y : ys) (x : zs) where
  append :: HList (x : xs) -> HList (y : ys) -> HList (x : zs)
  append (x `HCons` xs) (y `HCons` ys) = x `HCons` append xs (y `HCons` ys)

type Lookup :: Type -> [Type] -> Constraint
class Lookup x xs where
  lookup :: HList xs -> x

instance {-# OVERLAPPING #-} Lookup x (x : xs) where
  lookup :: HList (x : xs) -> x
  lookup (x `HCons` _) = x

instance Lookup x xs => Lookup x (y : xs) where
  lookup :: HList (y : xs) -> x
  lookup (_ `HCons` xs) = lookup xs

type Subset :: [Type] -> [Type] -> Constraint
class Subset sub sup where
  subset :: HList sup -> HList sub

instance Subset '[] sup where
  subset :: HList sup -> HList '[]
  subset _ = HNil

instance (Lookup x sup, Subset sub sup) => Subset (x : sub) sup where
  subset :: HList sup -> HList (x : sub)
  subset sup = lookup sup `HCons` subset sup
