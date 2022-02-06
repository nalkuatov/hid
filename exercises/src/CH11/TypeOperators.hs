{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module CH11.TypeOperators where

data a + b = Inl a | Inr b
  deriving Show

data a * b = a :*: b
  deriving Show

infixl 6 +
infixl 7 *

test :: Bool :*: String
test = False :*: "prodigious"

test2 :: Bool + String
test2 = Inl False

first :: a * b -> a
first (a :*: _) = a

second :: a * b -> b
second (_ :*: b) = b
