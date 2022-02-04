{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module CH11.Proxies where

import Data.Kind
import Data.Proxy

newtype Temp unit = Temp Double deriving
  (Num, Fractional)

data F
data C

paperBurning :: Temp F
paperBurning = 451

zeroAbsolute :: Temp C
zeroAbsolute = -273.15

f2c :: Temp F -> Temp C
f2c (Temp f) = Temp $ (f - 32) * 5 / 9

nonsense :: Temp Bool
nonsense = 0

class UnitName u where
  name :: Proxy u -> String

instance UnitName F where
  name :: Proxy F -> String
  name _ = "F"

instance UnitName C where
  name :: Proxy C -> String
  name _ = "C"

instance UnitName unit => UnitName (Temp unit) where
  name _ = name (Proxy :: Proxy unit)

instance UnitName unit => Show (Temp unit) where
  show (Temp t) = show t <> "°" <> name (Proxy :: Proxy unit)

instance UnitName Temp where
  name _ = "_unspecified unit_"

data K

instance UnitName K where
  name _ = "K"

unit :: forall unit. UnitName unit => Temp unit -> String
unit (Temp t) = name (Proxy :: Proxy unit)
