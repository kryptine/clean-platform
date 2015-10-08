implementation module Data.Error

import StdMisc
import Data.Functor, Data.Maybe
import Control.Monad
import Control.Applicative
	
liftError :: !(MaybeError .a .b) -> (MaybeError .a .c)
liftError	(Error a)	= Error a
liftError	(Ok _)		= abort "Data.Error.liftError: argument is Ok"

mb2error :: !e !(Maybe a) -> MaybeError e a
mb2error error mbV = maybe (Error error) Ok mbV

okSt :: *st (.a *st -> *st) (MaybeError .e .a) -> *st
okSt st f (Error _) = st
okSt st f (Ok x)    = f x st

error2mb :: !(MaybeError e a) -> Maybe a
error2mb (Error _) = Nothing
error2mb (Ok a)  = Just a

seqErrors :: !(MaybeError e a) (a -> MaybeError e b) -> MaybeError e b
seqErrors a bfunc = case a of
	Ok a	= bfunc a
	Error e	= Error e

combineErrors :: !(MaybeError e a) (MaybeError e b) (a b -> MaybeError e c) -> MaybeError e c
combineErrors a b combf = case a of
	Error e = Error e
	Ok a = case b of
		Error e	= Error e
		Ok b	= combf a b
		
seqErrorsSt :: !(.st -> (MaybeError e a,!.st)) (a .st -> u:(!MaybeError e b, !.st)) !.st -> v:(MaybeError e b, !.st), [u <= v]
seqErrorsSt aop bop st
	# (a,st) = aop st
	= case a of
		Error e = (Error e,st)
		Ok a	= bop a st

		
combineErrorsSt :: !(.st -> (!MaybeError e a, !.st)) (.st -> (!MaybeError e b, !.st)) (a b -> MaybeError e c) !.st -> (!MaybeError e c, !.st)
combineErrorsSt aop bop combf st
	# (a,st) = aop st
	= case a of
		Error e = (Error e,st)
		Ok a
			# (b,st) = bop st
			= case b of
				Error e = (Error e, st)
				Ok b	= (combf a b, st)
