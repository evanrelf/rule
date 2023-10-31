{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}

-- {-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Rule.Test where

import Rule
import Rule.Env.Do qualified as E

env1 :: Env ["name" ::: String, "age" ::: Int]
env1 =
    insert #name "Evan"
  $ insert #age 26
  $ empty

env2 ::
  Env
    [ "name" ::: String
    , "age" ::: Int
    , "likesDogs" ::: Bool
    ]
env2 = E.do
  #name =. "Evan"
  #age =. 26
  #likesDogs =. True
