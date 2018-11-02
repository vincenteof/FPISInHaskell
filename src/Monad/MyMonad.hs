module Monad.MyMonad where

class MyFunctor f where
  myMap :: f a -> (a -> b) -> f b

distribute :: (MyFunctor f) => f (a, b) -> (f a, f b)
distribute fab = (myMap fab fst, myMap fab snd)

codistribute :: (MyFunctor f) => Either (f a) (f b) -> f (Either a b)
-- codistribute (Left (f x)) = f (Left x)
-- is wrong, because f is just a type, not a constructor
codistribute (Left fa) = myMap fa Left
codistribute (Right fb) = myMap fb Right


class MyMonad m where
  unit :: a -> m a
  flatMap :: m a -> (a -> m b) -> m b
  map' :: m a -> (a -> b) -> m b
  map' ma f = flatMap ma (unit . f)
  map2 :: m a -> m b -> (a -> b -> c) -> m c
  map2 ma mb f = flatMap ma (map' mb . f)
  