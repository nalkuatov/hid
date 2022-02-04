{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CH11.TypeApplications where

class UnitName u where
  name :: String

data F
data C

newtype Temp u = Temp Double
  deriving (Num, Fractional)

instance UnitName C where
  name = "C"

instance UnitName F where
  name = "F"

instance UnitName u => UnitName (Temp u) where
  name = "*" ++ name @u

instance UnitName u => Show (Temp u) where
  show (Temp t) = show t ++ "*" ++ name @u

unit :: forall u. UnitName u => Temp u -> String
unit _ = name @(Temp u)
