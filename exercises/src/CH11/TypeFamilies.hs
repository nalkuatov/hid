{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module CH11.TypeFamilies where

import Unsafe.Coerce
import Data.Char

type family Simplify t

type instance Simplify Int = Integer
type instance Simplify Integer = Integer
type instance Simplify String = String
type instance Simplify Char = String
type instance Simplify Double = Integer
type instance Simplify Bool = String

class Simplifier t where
  simplify :: t -> Simplify t

instance Simplifier Int where
  simplify = fromIntegral

instance Simplifier String where
  simplify = id

instance Simplifier Char where
  simplify = (: "")

instance Simplifier Double where
  simplify = round

instance Simplifier Integer where
  simplify = id

instance Simplifier Bool where
  simplify = show

---------------------------------
-- convenient 'print' alternative
---------------------------------

newtype UnescapingChar =
  UnescapingChar { toChar :: Char }

---------------
-- the following implementation is copied
-- the book
instance Show UnescapingChar where
  showsPrec _ (UnescapingChar '\'') = showString "'\\''"
  showsPrec _ (UnescapingChar c) =
    showChar '\'' . showLitChar' c . showChar '\''
  showList cs = showChar '"'
    . showLitString' (map toChar cs)
    . showChar '"'

showLitChar' :: Char -> ShowS
showLitChar' c s | c > '\DEL' = showChar c s
showLitChar' c s = showLitChar c s

showLitString' :: String -> ShowS
showLitString' [] s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c : cs) s = showLitChar' c (showLitString' cs s)
---------------

type family UnescapingTF (a :: k) :: k where
  UnescapingTF Char = UnescapingChar
  UnescapingTF (t b :: k) = (UnescapingTF t) (UnescapingTF b)
  UnescapingTF a = a

class ToUnescaping a where
  toUnescaping :: a -> UnescapingTF a

instance Show a => ToUnescaping a where
  toUnescaping = unsafeCoerce

type UnescapingShow t = (ToUnescaping t, Show (UnescapingTF t))

ushow :: UnescapingShow a => a -> String
ushow = show . toUnescaping

uprint :: UnescapingShow a => a -> IO ()
uprint = putStrLn . ushow
