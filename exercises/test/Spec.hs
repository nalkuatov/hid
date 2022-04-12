{-# LANGUAGE ImportQualifiedPost #-}

import Hedgehog
import Hedgehog.Range qualified as Range
import Hedgehog.Gen   qualified as Gen

import Control.Applicative

sample_property :: Property
sample_property = property $ do
  (a, b) <- forAll $
    liftA2 (,) (Gen.int32 $ Range.linear 1 10) (Gen.int32 $ Range.linear 1 10)
  a + b === b + a

main :: IO Bool
main = checkParallel $
  Group "sample" [("samples", sample_property)]
