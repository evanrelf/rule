{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rule.HList where

import Data.Kind (Constraint, Type)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

infixr 5 `HCons`

type (<>) :: [k] -> [k] -> [k]
type family (<>) xs ys where
  (<>) '[] '[] = '[]
  (<>) (x : xs) '[] = x : xs
  (<>) '[] (y : ys) = y : ys
  (<>) (x : xs) (y : ys) = (x : (xs <> (y : ys)))

infixr 6 <>

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
