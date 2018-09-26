implementation module iTasksTTY

import StdEnv

import Data.Func
import Text

import iTasks
import iTasks.Internal.IWorld
import iTasks.Internal.SDS
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskState

import TTY

:: *Resource | TTYd String *TTY

derive class iTask TTYSettings, Parity, BaudRate, ByteSize

syncSerialChannel :: Timespec TTYSettings (b -> String) (String -> (Either String [a], String)) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
syncSerialChannel poll opts enc dec rw = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld
	# (mtty, iworld=:{world,resources}) = getResource iworld
	= case mtty of
		[] = case TTYopen opts iworld.world of
			(False, _, world)
				# (err, world) = TTYerror world
				= (exc err, {iworld & world=world})
			(True, tty, world)
			# (merr, iworld) = readRegister taskId ticker {iworld & world=world, resources=[TTYd opts.devicePath tty:resources]}
			| isError merr = (ExceptionResult (fromError merr), iworld)
			= (ValueResult
				NoValue
				{TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
				rep
				(TCBasic taskId ts (DeferredJSONNode $ JSONString "") False)
			  , iworld)
		_ = (exc "This tty was already open", iworld)

	eval _ _ tree=:(TCBasic taskId ts (DeferredJSONNode (JSONString acc)) _) iworld
	# (mtty, iworld) = getResource iworld
	= case mtty of
		[] = (exc"TTY resource lost", iworld)
		[_,_:_] = (exc "Multiple matching resources", iworld)
		[TTYd dp tty]
			# (merr, iworld) = readRegister taskId ticker iworld
			| isError merr = (ExceptionResult (fromError merr), iworld)
			# (merr, iworld=:{resources}) = read rw iworld
			| isError merr = (ExceptionResult (fromError merr), iworld)
			= case fromOk merr of
				//We need to stop
				(_,_,True) =
					(ValueResult
						(Value () True)
						{TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
						rep
						(TCDestroy tree)
					, {iworld & resources=[TTYd dp tty:resources]})
				(r,s,ss)
					# tty = foldr TTYwrite tty $ reverse $ map enc s
					# (merr, tty) = readWhileAvailable tty
					| isError merr = (exc (fromError merr), iworld)
					# iworld = {iworld & resources=[TTYd dp tty:iworld.resources]}
					= case dec (acc +++ toString (fromOk merr)) of
						(Left err, newacc) = (exc ("Error while parsing: " +++ join " " [toString (toInt c)\\c<-:acc+toString (fromOk merr)]), iworld)
						(Right msgs, newacc)
							# (merr, iworld) = if (msgs =: [] && s =: [])
								(Ok (), iworld)
								(write (r++msgs, [], False) rw iworld)
							| isError merr = (ExceptionResult (fromError merr), iworld)
							= (ValueResult
								NoValue
								{TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
								rep
								(TCBasic taskId ts (DeferredJSONNode $ JSONString newacc) False)
							  , iworld)

	eval event evalOpts tree=:(TCDestroy _) iworld=:{IWorld|resources,world}
	# (mtty, iworld) = getResource iworld
	= case mtty of
		[] = (exc "This tty was already closed", iworld)
		[_,_:_]  = (exc "Multiple matching resources", iworld)
		[TTYd _ tty]
		# (ok, world) = TTYclose tty iworld.world
		# iworld & world = world
		| not ok = (exc "Couldn't close device", iworld)
		= (DestroyedResult, iworld)

	rep = ReplaceUI $ stringDisplay $ "Serial client " <+++ opts.devicePath
	ticker = sdsFocus {start=zero,interval=poll} iworldTimespec
	getResource = iworldResource (\t=:(TTYd p _)->(p == opts.devicePath, t))
	exc = ExceptionResult o exception

readWhileAvailable :: !*TTY -> (MaybeError String [Char], !*TTY)
readWhileAvailable tty
# (available, error, tty) = TTYavailable tty
| error = (Error "TTY device disconnected", tty)
| not available = (Ok [], tty)
# (c, tty) = TTYread tty
# (merr, tty) = readWhileAvailable tty
| isError merr = (merr, tty)
= (Ok [toChar c:fromOk merr], tty)
