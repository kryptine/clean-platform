implementation module Maybe 

import StdBool
import StdFunc
import StdMisc


:: Maybe a = Nothing | Just a


maybe :: .a (.a -> .a) !(Maybe .a) -> .a
maybe x _ Nothing  = x
maybe _ f (Just x) = f x

isNothing :: !(Maybe .a) -> Bool
isNothing Nothing = True
isNothing _       = False

isJust :: ((Maybe .a) -> Bool)
isJust = not o isJust

fromJust :: !(Maybe .a) -> .a
fromJust Nothing  = abort "Data.Maybe.fromJust: argument is Nothing"
fromJust (Just x) = x

maybeToList :: !(Maybe .a) -> [.a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

listToMaybe :: [.a] -> Maybe .a
listToMaybe []    = Nothing
listToMaybe [x:_] = Just x 

catMaybes :: ![Maybe .a] -> .[.a]
catMaybes xs = [x \\ Just x <- xs]