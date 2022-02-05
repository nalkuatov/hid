{-# LANGUAGE TypeFamilies #-}

module CH11.TypeFamilies where

type family Simplify t

type instance Simplify Int = Integer
type instance Simplify String = String
type instance Simplify Double = Integer
type instance Simplify Float = Integer
type instance Simplify Bool = String
