{-# LANGUAGE FlexibleInstances #-}

module Monoid.MyMonoid where

class MyMonoid m where
  op :: m -> m -> m
  zero :: m

instance MyMonoid String where
  op s1 s2 = s1 ++ s2
  zero = ""

instance MyMonoid [a] where
  op xs ys = xs ++ ys
  zero = []

instance MyMonoid Int where
  op x y = x + y
  zero = 0

newtype IntMul = IntMul { getInt :: Int }
instance MyMonoid IntMul where
  op x y = IntMul (getInt x * getInt y)
  zero = IntMul 1

instance MyMonoid Bool where
  op = (||)
  zero = False

newtype BoolAnd = BoolAnd { getBool :: Bool }
instance MyMonoid BoolAnd where
  op x y = BoolAnd (getBool x && getBool y)
  zero = BoolAnd True