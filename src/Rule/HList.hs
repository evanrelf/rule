{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Rule.HList where

import Data.Kind (Type)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

infixr 5 `HCons`

class Append xs ys zs | xs ys -> zs where
  append :: HList xs -> HList ys -> HList zs

instance Append xs '[] xs where
  append :: HList xs -> HList '[] -> HList xs
  append xs HNil = xs

instance Append '[] ys ys where
  append :: HList '[] -> HList ys -> HList ys
  append HNil ys = ys

instance Append xs ys zs => Append (x : xs) ys (x : zs) where
  append :: HList (x : xs) -> HList ys -> HList (x : zs)
  append (x `HCons` xs) ys = x `HCons` append xs ys
