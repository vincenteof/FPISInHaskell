module Laziness.Stream 
(
    Stream(..),
    foldRight,
    exists,
    forAll,
    takeWhileBy,
    headOption
) where

-- Actually there is no need to have a seperate module to deal with laziness,
-- because Haskell is default lazy.

import qualified ErrorHandling.Option as O

data Stream a = Empty | Cons a (Stream a)

foldRight :: Stream a -> b -> (a -> b -> b) -> b
foldRight Empty z _ = z
foldRight (Cons x xs) z f = f x (foldRight xs z f)

-- how could i know it breaks when the condition is first statisfied
exists :: Stream a -> (a -> Bool) -> Bool
exists xs p = foldRight xs False (\ cur acc -> p cur || acc)

forAll :: Stream a -> (a -> Bool) -> Bool
forAll xs p = not $ exists xs (not . p)

takeWhileBy :: Stream a -> (a -> Bool) -> Stream a
takeWhileBy xs p = foldRight xs Empty (\ cur acc -> 
    if p cur
        then Cons cur acc
        else Empty)

headOption :: Stream a -> O.Option a
headOption xs = foldRight xs O.None (\ cur acc -> O.Some cur)

map :: Stream a -> (a -> b) -> Stream b
map xs f = foldRight xs Empty (\ cur acc -> Cons (f cur) acc)

filter :: Stream a -> (a -> Bool) -> Stream a
filter xs p = foldRight xs Empty (\ cur acc -> 
    if p cur
        then Cons cur acc
        else acc)

append :: Stream a -> Stream a -> Stream a
append xs ys = foldRight xs ys Cons

flatMap :: Stream a -> (a -> Stream b) -> Stream b
flatMap xs f = foldRight xs Empty (\ cur acc -> append (f cur) acc)