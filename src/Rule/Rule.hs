{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module Rule.Rule where

import Rule.Env (Env)

newtype Rule env a = Rule (Env env -> a)

runRule :: Env env -> Rule env a -> a
runRule env (Rule f) = f env
