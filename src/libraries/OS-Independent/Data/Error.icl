implementation module Error

import StdMisc

isOk		:: !(MaybeError a b) -> Bool
isOk		(Ok _) 		= True
isOk		(Error _)	= False

isError		:: !(MaybeError a b) -> Bool
isError		(Ok _) 		= False
isError		(Error _)	= True

fromOk		:: !(MaybeError a b) -> b
fromOk		(Ok b) 		= b
fromOk		(Error _)	= abort "Data.Error.fromOk: argument is Error"

fromError	:: !(MaybeError a b) -> a
fromError	(Error a) 	= a
fromError	(Ok _)		= abort "Data.Error.fromError: argument is Ok"

liftError :: !(MaybeError a b) -> (MaybeError a c)
liftError	(Error a)	= Error a
liftError	(Ok _)		= abort "Data.Error.liftError: argument is Ok"
