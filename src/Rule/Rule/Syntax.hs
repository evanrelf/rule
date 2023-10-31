{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Rule.Rule.Syntax where

import Control.Monad qualified as Base (join)
import Prelude (undefined)
import Prelude qualified as Base
import Rule.Env (Env' (..), Field (..), FieldName)
import Rule.HList (Append, type (<>), HList (..))
import Rule.Rule

get :: FieldName k -> Rule '[Field k v] v
get _ = Rule \(Env (Field _ v `HCons` _)) -> v

fmap :: (a -> b) -> Rule env a -> Rule env b
fmap = Base.fmap

(<*>) :: Rule env (a -> b) -> Rule env a -> Rule env b
(<*>) = (Base.<*>)

-- TODO: Implement `Subset` so that `zs` can satisfy `xs` and `ys` as a superset
-- of both.

(>>=) :: Rule xs a -> (a -> Rule ys b) -> Rule (xs <> ys) b
(>>=) _ _ = undefined

(>>) :: Rule xs a -> Rule ys b -> Rule (xs <> ys) b
(>>) _ _ = undefined

join :: Rule env (Rule env a) -> Rule env a
join = Base.join

pure :: a -> Rule '[] a
pure = Base.pure
