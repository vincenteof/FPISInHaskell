module Option 
(
    mapTo,
    flatMap,
    getOrElse,
    orElse,
    filterBy
) where

data Option a = Some a | None

mapTo :: Option a -> (a -> b) -> Option b
mapTo None _ = None
mapTo (Some a) f = Some (f a)

flatMap :: Option a -> (a -> Option b) -> Option b
flatMap None _= None
flatMap (Some a) f = f a

getOrElse :: Option a -> a -> a
getOrElse None x = x
getOrElse (Some a) _ =  a

orElse :: Option a -> Option a -> Option a
orElse None op = op
orElse op _ = op

filterBy :: Option a -> (a -> Bool) -> Option a
filterBy None _ = None
filterBy op@(Some a) f = if f a 
    then op
    else None

lift :: (a -> b) -> (Option a -> Option b)
lift f = (`mapTo` f)

