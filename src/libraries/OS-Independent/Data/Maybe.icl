implementation module Data.Maybe

import StdBool
import StdFunc
import StdMisc
import Data.Functor
import Control.Applicative
import Control.Monad

:: Maybe a = Nothing | Just a

instance == (Maybe x) | == x where
	(==) Nothing  maybe	= case maybe of
							Nothing -> True
							just    -> False
	(==) (Just a) maybe	= case maybe of
							Just b  -> a == b
							nothing -> False

instance Functor Maybe
where
	fmap f Nothing	= Nothing
	fmap f (Just a)	= Just (f a)

instance Applicative Maybe
where
	pure x            = Just x
	(<*>) Nothing  _  = Nothing
	(<*>) (Just f) ma = fmap f ma

instance Alternative Maybe
where
	empty           = Nothing
	(<|>) Nothing r = r
	(<|>) l       _ = l

instance Monad Maybe
where
	bind (Just x) k = k x
	bind Nothing  _ = Nothing

instance MonadPlus Maybe
where
	mzero = Nothing
	mplus Nothing ys = ys
	mplus xs      _  = xs

maybe :: w:b v:(.a -> w:b) !.(Maybe .a) -> w:b
maybe x _ Nothing  = x
maybe _ f (Just x) = f x

maybeSt :: *st (.a *st -> *st) !(Maybe .a) -> *st
maybeSt st _ Nothing  = st
maybeSt st f (Just x) = f x st

fromMaybe :: .a !(Maybe .a) -> .a
fromMaybe x mb = maybe x id mb

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

listToMaybe :: ![.a] -> Maybe .a
listToMaybe []    = Nothing
listToMaybe [x:_] = Just x 

catMaybes :: ![Maybe .a] -> .[.a]
catMaybes xs = [x \\ Just x <- xs]
