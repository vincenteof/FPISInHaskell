{-# LANGUAGE FlexibleInstances #-}

module Monoid.MyMonoid where

import Testing.Gen
import Data.List

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

newtype IntMul = IntMul{ getInt :: Int }

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

instance MyMonoid (Maybe a) where
  op (Just x) _ = Just x
  op Nothing y = y
  zero = Nothing

newtype EndoFunc a = EndoFunc { getFunc :: a -> a }

instance MyMonoid (EndoFunc a) where
  EndoFunc f `op` EndoFunc g = EndoFunc (f . g)
  zero = EndoFunc id

monoidLaws :: (Show a, Eq a, MyMonoid a) => Gen a -> Prop
monoidLaws gen =
  let threeMGen =
        gen `flatMap` (\x -> gen `flatMap` (\y -> gen `flatMap` (\z -> unit (x, y, z))))
      associativity = 
        forAll threeMGen (\(x, y, z) -> op x (op y z) == op (op x y) z)
      identity = 
        forAll gen (\x -> x `op` zero == zero `op` x)
  in associativity <&&> identity


concatenate :: (MyMonoid a) => [a] -> a -> a
concatenate xs z = foldl op z xs

foldMapWith :: (MyMonoid b) => [a] -> (a -> b) -> b
foldMapWith xs f = flip concatenate zero . map f $ xs

-- the type of `f` is `a -> (b -> b)`, and it will map a value with type `a` to a monoid 
foldrViaFoldMap :: (MyMonoid a) => (a -> b -> b) -> b -> [a] -> b
foldrViaFoldMap f z xs = getFunc (foldMapWith xs (EndoFunc . f)) z

newtype EndoFuncFlip a = EndoFuncFlip { getFunc' :: a -> a }
instance MyMonoid (EndoFuncFlip a) where
  EndoFuncFlip f `op` EndoFuncFlip g = EndoFuncFlip $ g . f
  zero = EndoFuncFlip id

foldlViaFoldMap :: (MyMonoid a) => (b -> a -> b) -> b -> [a] -> b
foldlViaFoldMap f z xs = getFunc' (foldMapWith xs (EndoFuncFlip . flip f)) z

foldMapV :: (MyMonoid b) => [a] -> (a -> b) -> b
foldMapV [] _ = zero
foldMapV [x] f = f x
foldMapV xs f = 
  let halfLen = length xs `div` 2
      (left, right) = splitAt halfLen xs
  in foldMapV left f `op` foldMapV right f

-- sorted :: [Int] -> Bool

data WC = Stub String | Part String Int String
-- instance MyMonoid WC where
--   Stub x `op` Stub y = 
--     let pred x y = (x == y && x == ' ') || (x /= ' ' && y /= ' ')
--         words = groupBy pred (x++y)
--         blankCount = length . filter (\s -> head s /= ' ') $ words
--     in 
--       if blankCount > 1
--         then Part (head words) (blankCount - 1) (last words)
--       else
--         Stub (x++y)
--   Part lx n ly `op` Part rx m ry = Part lx (n+m+1) ry
--   Stub s `op` Part l n r = Stub s `op` Stub l `op` Part "" n r
--   Part l n r `op` Stub s = Part l n "" `op` (Stub r `op` Stub s)
--   zero = Stub ""
instance MyMonoid WC where
  Stub x `op` Stub y = Stub (x++y)
  Stub s `op` Part l n r = Part (s++l) n r
  Part l n r `op` Stub s = Part l n (r++s)
  Part lx n ly `op` Part rx m ry = 
    let plusCount = if null (ly++rx) then 0 else 1
    in Part lx (n + plusCount + m) ry
  zero = Stub ""

wc :: Char -> WC 
wc c 
  | c == ' ' = Part "" 0 ""
  | otherwise = Stub [c]

unstub :: String -> Int
unstub s =  min (length s) 1

countForWords :: String -> Int
countForWords s = 
  case foldMapV s wc of
    Stub s -> unstub s
    Part l n r -> unstub l + n + unstub r
