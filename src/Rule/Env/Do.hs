{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rule.Env.Do where

import Rule.Env
import Rule.HList (Append (..))

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html

(=.) :: FieldName k -> v -> Env '[Field k v]
(=.) k v = insert k v empty

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
