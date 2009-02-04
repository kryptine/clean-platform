implementation module Maybe 

import StdBool
import StdFunc
import StdMisc


:: Maybe a = Nothing | Just a

fmap :: (.a -> .b) (Maybe .a) -> Maybe .b
fmap _ Nothing  = Nothing
fmap f (Just x) = Just (f x)

maybe :: .a (.a -> .a) !(Maybe .a) -> .a
maybe x _ Nothing  = x
maybe _ f (Just x) = f x

isNothing :: !(Maybe .a) -> Bool
isNothing Nothing = True
isNothing _       = False

isNothingU :: !u:(Maybe .a) -> (!Bool, !u:Maybe .a)
isNothingU Nothing = (True, Nothing)
isNothingU x       = (False, x)

isJust :: !(Maybe .a) -> Bool
isJust (Just _)    = True
isJust _           = False

isJustU :: !u:(Maybe .a) -> (!Bool, !u:Maybe .a)
isJustU (Just x)    = (True, Just x)
isJustU x           = (False, x)

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
