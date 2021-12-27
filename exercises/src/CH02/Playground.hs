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
  deriving (Eq, Enum, Bounded, Show)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, Show)

rotate :: Turn -> Direction -> Direction
rotate = undefined

orient :: Direction -> Direction -> Turn
orient = undefined

rotateMany :: [Turn] -> Direction -> Direction
rotateMany = undefined

orientMany :: [Direction] -> [Turn]
orientMany = undefined

rotateFromFile :: Direction -> FilePath -> IO Direction
rotateFromFile = undefined

main :: IO ()
main = undefined
