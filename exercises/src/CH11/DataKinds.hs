{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CH11.DataKinds where

import Data.Proxy
import GHC.TypeLits

data TempUnit = F | C

newtype Temp (u :: TempUnit) = Temp Double
  deriving (Num, Fractional)

class UnitName (u :: TempUnit) where
  name :: String

newtype Pointer (n :: Nat) =
  Pointer Integer deriving (Show, Num)

zero :: Pointer n
zero = Pointer 0

inc :: Pointer align -> Pointer align
inc (Pointer v) = Pointer $ v + 1

ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer value) = value * natVal (Proxy :: Proxy align)

maybePtr :: forall align. KnownNat align => Integer -> Maybe (Pointer align)
maybePtr p
  | remainder == 0 = Just $ Pointer p
  | otherwise = Nothing
  where
    (_, remainder) = divMod p $ natVal (Proxy :: Proxy align)
