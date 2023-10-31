{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Rule.Test where

import Rule
import Rule.Env.Syntax ((=.))
import Rule.Env.Syntax qualified as E
import Rule.Rule.Syntax (get)
import Rule.Rule.Syntax qualified as R

-- All type signatures are inferred correctly and completely!

env1 =
    insert #name "Evan"
  $ insert #age (26 :: Int)
  $ insert #likesDogs True
  $ empty

env2 = E.do
  #name =. "Evan"
  #age =. (26 :: Int)
  #likesDogs =. True

env3 = E.do
  #name "Evan"
  #age do 26 :: Int
  #likesDogs True

rule1 = R.do
  name :: String <- get #name
  age :: Int <- get #age
  R.pure (length name == age)
