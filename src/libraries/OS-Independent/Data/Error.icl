implementation module Error

import StdMisc

isOk		:: !(MaybeError a b) -> Bool
isOk		(Ok _) 		= True
isOk		(Error _)	= False

isError		:: !(MaybeError a b) -> Bool
isError		(Ok _) 		= False
isError		(Error _)	= True

fromOk		:: !(MaybeError a b) -> a
fromOk		(Ok a) 		= a
fromOk		(Error _)	= abort "Data.Error.fromOk: argument is Error"

fromError	:: !(MaybeError a b) -> b
fromError	(Error b) 	= b
fromError	(Ok _)		= abort "Data.Error.fromError: argument is Ok"

liftError :: !(MaybeError a b) -> (MaybeError c b)
liftError	(Error b)	= Error b
liftError	(Ok _)		= abort "Data.Error.liftError: argument is Ok"
