module ErrorHandling.Option
  ( Option(..)
  , mapTo
  , flatMap
  , getOrElse
  , orElse
  , filterBy
  , lift
  , mapTo2
  , sequenceOf
  , traverseWith
  ) where

import qualified DataStructures.List as L

data Option a
  = Some a
  | None

-- things to improve: 
-- make it a monad
mapTo :: Option a -> (a -> b) -> Option b
mapTo None _ = None
mapTo (Some a) f = Some (f a)

flatMap :: Option a -> (a -> Option b) -> Option b
flatMap None _ = None
flatMap (Some a) f = f a

getOrElse :: Option a -> a -> a
getOrElse None x = x
getOrElse (Some a) _ = a

orElse :: Option a -> Option a -> Option a
orElse None op = op
orElse op _ = op

filterBy :: Option a -> (a -> Bool) -> Option a
filterBy None _ = None
filterBy op@(Some a) f =
  if f a
    then op
    else None

lift :: (a -> b) -> (Option a -> Option b)
lift f x = x `mapTo` f

mapTo2 :: Option a -> Option b -> (a -> b -> c) -> Option c
mapTo2 None _ _ = None
mapTo2 _ None _ = None
mapTo2 (Some x) (Some y) f = Some (f x y)

sequenceOf :: L.List (Option a) -> Option (L.List a)
sequenceOf xs = traverseWith xs id

traverseWith :: L.List a -> (a -> Option b) -> Option (L.List b)
traverseWith xs f =
  L.foldRight xs (Some L.Nil) (\cur acc -> mapTo2 (f cur) acc L.Cons)
