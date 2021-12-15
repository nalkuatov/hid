module Playground where

type Append :: forall a. [a] -> [a] -> [a]

type family Append xs ys where
  Append '[]    ys = ys
  Append (x:xs) ys = x : Append xs ys
