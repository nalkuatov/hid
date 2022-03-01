{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NumericConversions where

import Data.IntCast
import Data.Word.Odd
import Data.Bits

newtype CustomWord =
  CustomWord { unCWord :: Word63 }
    deriving newtype (Show, Eq, Enum, Ord, Num, Real, Integral, Bits)

type instance IntBaseType CustomWord = 'FixedWordTag 63
