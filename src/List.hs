module List
(
List
) where

data List a = Nil | Cons a (List a)

-- things to improve: 
-- add synatx sugar for construction

tail :: List a -> List a
tail Nil = Nil
tail (Cons h t) = t

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

length :: List a -> Int
length xs = foldRight xs 0 (\ a b -> b + 1)

foldLeft :: List a -> b -> (b -> a -> b) -> b
foldLeft Nil z _ = z
foldLeft (Cons x xs) z f = foldLeft xs (f z x) f 

flattern :: List (List a) -> List a
flattern xs = foldRight xs Nil append

