{-# LANGUAGE DeriveAnyClass #-}

module CH02.Playground where

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

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
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

rotateFromFile :: Direction -> FilePath -> IO Direction
rotateFromFile = undefined

main :: IO ()
main = undefined
