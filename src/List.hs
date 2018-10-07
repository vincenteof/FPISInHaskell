module List
(
    List,
    tailPart,
    setHead,
    remove,
    removeWhile,
    append,
    exceptLast,
    foldRight,
    size,
    foldLeft,
    flattern,
    mapTo,
    filterBy,
    flatMap,
    zipBy,
    hasSubsequence
) where

data List a = Nil | Cons a (List a)

-- things to improve: 
-- add synatx sugar for construction

tailPart :: List a -> List a
tailPart Nil = Nil
tailPart (Cons h t) = t

setHead :: List a -> a -> List a
setHead Nil _ = Nil
setHead (Cons x xs) y = Cons y xs

remove :: List a -> Int -> List a
remove Nil _ = Nil
remove xs 0 = xs
remove (Cons x xs) n = remove xs (n-1)

removeWhile :: List a -> (a -> Bool) -> List a
removeWhile Nil _ = Nil
removeWhile ys@(Cons x xs) p = if p x 
    then removeWhile xs p 
    else ys

append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)

exceptLast :: List a -> List a
exceptLast Nil = Nil
exceptLast (Cons x Nil) = Nil
exceptLast (Cons x xs) = Cons x (exceptLast xs) 

foldRight :: List a -> b -> (a -> b -> b) -> b
foldRight Nil z _ = z
foldRight (Cons x xs) z f = f x (foldRight xs z f)

size :: List a -> Int
size xs = foldRight xs 0 (\ a b -> b + 1)

foldLeft :: List a -> b -> (b -> a -> b) -> b
foldLeft Nil z _ = z
foldLeft (Cons x xs) z f = foldLeft xs (f z x) f 

flattern :: List (List a) -> List a
flattern xs = foldRight xs Nil append

mapTo :: List a -> (a -> b) -> List b
mapTo Nil _ = Nil
mapTo (Cons x xs) f = Cons (f x) (mapTo xs f)

filterBy :: List a -> (a -> Bool) -> List a
filterBy Nil _ = Nil
filterBy (Cons x xs) f = if f x
    then Cons x (filterBy xs f)
    else filterBy xs f

flatMap :: List a -> (a -> List b) -> List b
flatMap Nil _ = Nil
flapMap (Cons x xs) f = append (f x) (flatMap xs f)

zipBy :: List a -> List b -> (a -> b -> c) -> List c
zipBy Nil _ _ = Nil
zipBy _ Nil _ = Nil
zipBy (Cons x xs) (Cons y ys) f = Cons (f x y) (zipBy xs ys f)


startsWith :: (Eq a) => List a -> List a -> Bool
startsWith _ Nil = True
startsWith Nil _ = False
startsWith (Cons x xs) (Cons y ys) = (x == y) && startsWith xs ys

hasSubsequence :: (Eq a) => List a -> List a -> Bool
hasSubsequence Nil ys = startsWith Nil ys 
hasSubsequence zs@(Cons x xs) ys = startsWith zs ys || hasSubsequence xs ys