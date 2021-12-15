module CH02.Playground where

class (Enum a, Bounded a) => CyclicEnum a where
  csucc :: a -> a

data Direction = North | South | East | West
