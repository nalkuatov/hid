{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CH12.Playground where

import Data.Aeson
import Data.Coerce
import GHC.Generics

newtype Age = Age Int
  deriving stock (Eq, Show, Generic)
  deriving newtype Num
  deriving anyclass ToJSON

age :: Age
age = 33

test = do
  print age
  print $ encode $ age

toAges :: [Int] -> [Age]
toAges = map Age

toAges' :: [Int] -> [Age]
toAges' = coerce
