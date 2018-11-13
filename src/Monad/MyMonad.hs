module Monad.MyMonad where

import Control.Monad

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

flatMapViaCompose :: (MyMonad m) => m a -> (a -> m b) -> m b
flatMapViaCompose ma f = compose (const ma) f ()

join' :: (MyMonad m) => m (m a) -> m a
join' mma = mma `flatMap` id

flatMapViaJoinAndMap :: (MyMonad m) => m a -> (a -> m b) -> m b
flatMapViaJoinAndMap ma famb = join' . flip map' famb $ ma 

composeViaJoinAndMap :: (MyMonad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
composeViaJoinAndMap famb fbmc x = 
  join' . _map fbmc . join' . _map famb $ unit x
  where _map = flip map'

newtype Id a = Id { value :: a }
instance MyMonad Id where
  unit = Id
  (Id x) `flatMap` f = f x


newtype MyState s a = MyState { myRun :: s -> (a, s) }

instance Functor (MyState s) where
  fmap = liftM

instance Applicative (MyState s) where
  pure = return
  (<*>) = ap

-- Here we use official `Monad` class, 
-- because we want to use `do` syntax for monad instead of nested function applications
instance Monad (MyState s) where
  return x = MyState (\s -> (x, s))
  MyState runF >>= f = MyState (\s -> 
    let (x, newS) = runF s
        MyState newRunF = f x
    in newRunF newS)

getState :: MyState s s
getState = MyState (\s -> (s, s))

setState :: s -> MyState s ()
setState newS = MyState $ const ((), newS)

state1 = MyState $ \s -> (s * 2, s + 2) :: (Int, Int)
state2 = MyState $ \s -> (s * 2, s + 3) :: (Int, Int) 


zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs = 
  let f acc a = (do
        ysAcc <- acc
        n <- getState
        _ <- setState (n + 1)
        return ((n, a):ysAcc))
      z = return []
      stateF = foldl f z xs
      result = myRun stateF 0
  in reverse . fst $ result


-- The context of MyReader is that we can treat it as if it has already be applied and get the result
-- `sequence` returns another reader which returns an array of the result of each reader applied to the argument
-- `join` flatten a reader, and argument to it become both part of the inner function and the argument
-- `replicateM` is almost the same as `sequence` 
newtype MyReader r a = MyReader { runReader :: r -> a }
instance Monad (MyReader r) where
  return x = MyReader $ const x
  -- MyReader readerF >>= f = MyReader (\x -> runReader (f (readerF x)) x)
  MyReader readerF >>= f = MyReader (\x -> (runReader . f . readerF) x x)

instance Functor (MyReader r) where
  fmap = liftM
  
instance Applicative (MyReader r) where
  pure = return
  (<*>) = ap