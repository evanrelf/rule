{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module Rule.Rule where

import Rule.Env (Env)

newtype Rule env a = Rule (Env env -> a)
  deriving stock (Functor)

instance Applicative (Rule env) where
  pure :: a -> Rule env a
  pure x = Rule \_ -> x

  (<*>) :: Rule env (a -> b) -> Rule env a -> Rule env b
  (<*>) = undefined

instance Monad (Rule env) where
  (>>=) :: Rule env a -> (a -> Rule env b) -> Rule env b
  (>>=) = undefined

runRule :: Env env -> Rule env a -> a
runRule env (Rule f) = f env
