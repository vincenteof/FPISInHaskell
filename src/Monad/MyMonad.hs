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
  

instance MyMonad Maybe where
  unit = Just
  flatMap Nothing _ = Nothing
  flatMap (Just x) f = f x

instance MyMonad [] where
  unit x = [x]
  flatMap xs f = concatMap f xs


-- mySequence for [] is just like multi-loop
-- mySequence for Maybe will yield Nothing is one of the Maybe values are Nothing
mySequence :: (MyMonad m) => [m a] -> m [a]
mySequence = foldr (\mx mxs -> map2 mx mxs (:)) (unit []) 

myTraverse :: (MyMonad m) => [a] -> (a -> m b) -> m [b] 
myTraverse xs f = foldr (\x mxs -> map2 (f x) mxs (:)) (unit []) xs

myReplicateM :: (MyMonad m) => Int -> m a -> m [a]
myReplicateM n =  mySequence . replicate n

myProduct :: (MyMonad m) => m a -> m b -> m (a, b)
myProduct ma mb = map2 ma mb (\x y -> (x, y))

-- m Bool is a monad whose content is Bool type
-- for Maybe, if any pred return Nothing, whole result is nothing, otherwise its behaviour is same as `filter`
-- for [], it is like multi-loop, 
-- if some elem return [True, False], the reusult will fork 2 branches, one contains the elem and the other not  
myFilterM :: (MyMonad m) => [a] -> (a -> m Bool) -> m [a]
myFilterM xs f =
  foldr
    (\x mxs ->
       map2
         (f x)
         mxs
         (\pred ys ->
            if pred
              then x : ys
              else ys))
    (unit [])
    xs


compose :: (MyMonad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
compose f g x = f x `flatMap` g

newtype MyState s a = MyState { run :: s -> (a, s) }



