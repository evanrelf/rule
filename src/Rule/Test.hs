{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Rule.Test where

import Rule

env :: Env ["name" ::: String, "age" ::: Int]
env =
    insert #name "Evan"
  $ insert #age 26
  $ empty
