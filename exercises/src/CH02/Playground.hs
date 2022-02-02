{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module CH02.Playground where

import Fmt
import Control.Monad
import Control.Monad.IO.Class
import System.Random.Stateful
import System.Random

class (Eq a, Enum a, Bounded a) => CyclicEnum a where

  cpred :: a -> a
  cpred a
    | a == minBound = maxBound
    | otherwise     = pred a

  csucc :: a -> a
  csucc a
    | a == maxBound = minBound
    | otherwise     = succ a

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, Show, CyclicEnum)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, Show)

instance Semigroup Turn where
  TNone   <> a       = a
  TLeft   <> TLeft   = TAround
  TLeft   <> TRight  = TNone
  TLeft   <> TAround = TRight
  TRight  <> TAround = TLeft
  TRight  <> TRight  = TAround
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty = TNone

deriving instance Read Turn
deriving instance Read Direction
deriving instance Ord Turn

instance Buildable Direction where
  build North = "N"
  build South = "S"
  build West  = "W"
  build East  = "E"

instance Buildable Turn where
  build TNone   = "--"
  build TRight  = "->"
  build TLeft   = "<-"
  build TAround = "||"

rotate :: Turn -> Direction -> Direction
rotate TNone   = id
rotate TLeft   = cpred
rotate TRight  = csucc
rotate TAround = cpred . cpred

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient a b = head $ filter
  ((== b) . flip rotate a) every

rotateMany :: [Turn] -> Direction -> Direction
rotateMany turns = rotate $ mconcat turns

rotateManySteps :: [Turn] -> Direction -> [Direction]
rotateManySteps turns a = scanl (flip rotate) a turns

orientMany :: [Direction] -> [Turn]
orientMany ds@(a : b : rest) = orient a b : (orientMany $ b : rest)
orientMany _ = []

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir file = do
  turns <- map read . lines <$> readFile file
  fmt $ nameF "The turns are: " $ unwordsF turns
  let result = rotateMany turns dir
  let dirs   = rotateManySteps turns dir
  fmtLn $ "The resultant direction is: "+||result||+""
  fmtLn $ nameF "Intermediate directions are: " $ unwordsF dirs

---------------------------------------------
-- RANDOMNESS
---------------------------------------------

instance UniformRange Direction where
  uniformRM (lo, hi) rng = do
    rn <- uniformRM (fromEnum lo, fromEnum hi) rng
    pure $ toEnum rn

instance Uniform Direction where
  uniformM =
    uniformRM (minBound, maxBound)

uniformIO :: (MonadIO m, Uniform a) => m a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

writeRandomFile :: (Uniform a, Show a)
  => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen file = do
  values <- gen n
  writeFile file $ unlines $ map show values

main :: IO ()
main = undefined
