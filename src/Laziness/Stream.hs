module Laziness.Stream
  ( Stream(..)
  , foldRight
  , exists
  , forAll
  , takeWhileBy
  , headOption
  , mapTo
  , filter
  , append
  , flatMap
  , constant
  , from
  , fibs
  , unfold
  , fibsViaUnfold
  , mapViaUnfold
  , filterBy
  ) where

-- Actually there is no need to have a seperate module to deal with laziness,
-- because Haskell is default lazy.
import qualified ErrorHandling.Option as O

data Stream a
  = Empty
  | Cons a
         (Stream a)

foldRight :: Stream a -> b -> (a -> b -> b) -> b
foldRight Empty z _ = z
foldRight (Cons x xs) z f = f x (foldRight xs z f)

-- how could i know it breaks when the condition is first statisfied
exists :: Stream a -> (a -> Bool) -> Bool
exists xs p = foldRight xs False (\cur acc -> p cur || acc)

forAll :: Stream a -> (a -> Bool) -> Bool
forAll xs p = not $ exists xs (not . p)

takeWhileBy :: Stream a -> (a -> Bool) -> Stream a
takeWhileBy xs p =
  foldRight
    xs
    Empty
    (\cur acc ->
       if p cur
         then Cons cur acc
         else Empty)

headOption :: Stream a -> O.Option a
headOption xs = foldRight xs O.None (\cur acc -> O.Some cur)

mapTo :: Stream a -> (a -> b) -> Stream b
mapTo xs f = foldRight xs Empty (\cur acc -> Cons (f cur) acc)

filterBy :: Stream a -> (a -> Bool) -> Stream a
filterBy xs p =
  foldRight
    xs
    Empty
    (\cur acc ->
       if p cur
         then Cons cur acc
         else acc)

append :: Stream a -> Stream a -> Stream a
append xs ys = foldRight xs ys Cons

flatMap :: Stream a -> (a -> Stream b) -> Stream b
flatMap xs f = foldRight xs Empty (\cur acc -> append (f cur) acc)

constant :: a -> Stream a
constant n = Cons n (constant n)

from :: Int -> Stream Int
from n = Cons n (from $ n + 1)

fibs :: Stream Int
fibs = fibsFrom 0

fibsFrom :: Int -> Stream Int
fibsFrom n = Cons (fibsOn n) (fibsFrom $ n + 1)

fibsOn :: Int -> Int
fibsOn 0 = 1
fibsOn 1 = 1
fibsOn n = fibsOn (n - 1) + fibsOn (n - 2)

unfold :: s -> (s -> O.Option (a, s)) -> Stream a
unfold z f =
  let result = f z
   in case result of
        O.None -> Empty
        O.Some (x, s') -> Cons x (unfold s' f)

-- It seems running fatser than the previous one
fibsViaUnfold :: Stream Int
fibsViaUnfold = unfold (0, 1) (\(m, n) -> O.Some (m + n, (n, m + n)))

mapViaUnfold :: Stream a -> (a -> b) -> Stream b
mapViaUnfold xs f = unfold xs g
  where
    g Empty = O.None
    g (Cons y ys) = O.Some (f y, ys)
