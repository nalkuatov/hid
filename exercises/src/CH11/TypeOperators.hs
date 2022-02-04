{-# LANGUAGE TypeOperators #-}

module CH11.TypeOperators where

data a + b = Inl a | Inr b
  deriving Show

-- |
-- using * as a type constructor fails to compile with
-- __malformed head of type or typeclass__
-- I've no idea why.
-- the book uses @data a * b = a :*: b@
data a :*: b = a :*: b
  deriving Show

infixl 6 +
infixl 7 :*:

test :: Bool :*: String
test = False :*: "prodigious"

test2 :: Bool + String
test2 = Inl False

first :: a :*: b -> a
first (a :*: _) = a

second :: a :*: b -> b
second (_ :*: b) = b
