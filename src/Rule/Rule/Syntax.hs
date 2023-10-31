{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Rule.Rule.Syntax where

import Prelude hiding ((>>=), (>>), pure)
import Rule.Env (Env' (..), Field (..), FieldName)
import Rule.HList (Append, HList (..), Subset (..))
import Rule.Rule

get :: FieldName k -> Rule '[Field k v] v
get _ = Rule \(Env (Field _ v `HCons` _)) -> v

(>>=) :: (Append xs ys zs, Subset xs zs, Subset ys zs) => Rule xs a -> (a -> Rule ys b) -> Rule zs b
(>>=) ruleX ruleK = Rule \(Env env) ->
  let
    k = runRule (Env (subset env)) . ruleK
    x = runRule (Env (subset env)) ruleX
  in
    k x

(>>) :: (Append xs ys zs, Subset xs zs, Subset ys zs) => Rule xs a -> Rule ys b -> Rule zs b
(>>) _ ruleR = Rule \(Env env) -> runRule (Env (subset env)) ruleR

pure :: a -> Rule '[] a
pure x = Rule \_ -> x
