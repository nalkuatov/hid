{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CH02.Person where

import Data.String

data Person = Person String (Maybe Int)

deriving instance Show Person
deriving instance Read Person
deriving instance Eq Person

instance IsString Person where
  fromString name = Person name Nothing
