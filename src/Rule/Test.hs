{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Rule.Test where

import Rule

env :: HList ["name" ::: String, "age" ::: Int]
env =
  #name := "Evan" `HCons`
  #age := 26 `HCons`
  HNil
