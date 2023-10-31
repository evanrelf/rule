{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Rule.Env.Syntax where

import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol)
import Prelude (type (~))
import Rule.Env
import Rule.HList (Append (..))

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html

(=.) :: FieldName k -> v -> Env '[Field k v]
(=.) k v = insert k v empty

instance
  ( l ~ k
  , a ~ v
  , b ~ '[Field k v]
  , KnownSymbol k
  ) => IsLabel l (a -> Env b) where
  fromLabel = \v -> insert (FieldName (Proxy @k)) v empty

fmap :: ()
fmap = ()

(<*>) :: ()
(<*>) = ()

(>>=) :: ()
(>>=) = ()

(>>) :: Append xs ys zs => Env xs -> Env ys -> Env zs
(>>) (Env xs) (Env ys) = Env (append xs ys)

join :: ()
join = ()
