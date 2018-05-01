implementation module iTasksTTY

import System.OS
import TTY

import StdList
from StdFunc import o, flip
import StdMisc
import StdString
import Data.List
import qualified Data.Map as DM

import System.Directory
import iTasks
import Data.Tuple
import Control.Applicative
from Data.Func import $
from Text import class Text(startsWith), instance Text String

import iTasks.UI.Definition

import iTasks.Internal.IWorld
import iTasks.Internal.TaskState
import iTasks.Internal.Task
import iTasks.Internal.SDS
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskEval

:: *Resource | TTYd *(String, Int, *TTY)

derive class iTask TTYSettings, Parity, BaudRate, ByteSize

getTTYResource :: String *IWorld -> (Maybe *(*TTY, Int), *IWorld)
getTTYResource dp iw=:{resources}
# (mt, resources) = getTTYResource` resources
= (mt, {iw & resources=resources})
where
	getTTYResource` :: *[*Resource] -> (Maybe *(*TTY, Int), *[*Resource])
	getTTYResource` [] = (Nothing, [])
	getTTYResource` [TTYd (dpath, bgid, tty):xs]
	| dpath == dp = (Just (tty, bgid), xs)
	getTTYResource` [x:xs]
	# (mt, xs) = getTTYResource` xs
	= (mt, [x:xs])

liftWorld f = \iw=:{world}->{iw & world=f world}

syncSerialChannel :: TTYSettings (b -> String) (String -> (Either String [a], String)) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
syncSerialChannel opts enc dec rw = withShared "" \sh->Task $ eval sh
	where
		eval sh event evalOpts tree=:(TCInit taskId ts) iworld
		# (mtty, iworld) = getTTYResource opts.devicePath iworld
		= case mtty of
			Just (tty, _) = (ExceptionResult (exception "This tty was already open"), iworld)
			Nothing = case TTYopen opts iworld.world of
				(False, _, world)
				# (err, world) = TTYerror world
				= (ExceptionResult (exception err), {iworld & world=world})
				(True, tty, world)
				# iworld = {iworld & world=world}
				= case addBackgroundTask (BackgroundTask (serialDeviceBackgroundTask opts.devicePath enc dec sh rw)) iworld of
					(Error e, iworld) = (ExceptionResult (exception "background task couldn't be added"), iworld)
					(Ok bgid, iworld) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep (TCBasic taskId ts JSONNull False),
						{iworld & resources=[TTYd (opts.devicePath, bgid, tty):iworld.resources]})

		eval _ _ _ tree=:(TCBasic _ ts _ _) iworld
		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} rep tree, iworld)

		eval _ event evalOpts tree=:(TCDestroy _) iworld=:{IWorld|resources,world}
		# (mtty, iworld) = getTTYResource opts.devicePath iworld
		= case mtty of
			Nothing = (ExceptionResult (exception "This tty was already closed"), iworld)
			Just (tty, bgid)
			# (ok, world) = TTYclose tty iworld.world
			# iworld = {iworld & world=world}
			= case removeBackgroundTask bgid iworld of
				(Error e, iworld) = (ExceptionResult e, iworld)
				(Ok _, iworld) = (DestroyedResult, iworld)

		rep = ReplaceUI $ stringDisplay $ "Serial client " <+++ opts.devicePath

/*
 * The actual backgroundtask synchronizing the device
 *
 * @param Device path
 * @param encoding function
 * @param decoding function
 * @param shared channels
 * @param IWorld
 * @return Maybe an exception
 */
serialDeviceBackgroundTask :: String (b -> String) (String -> (Either String [a], String)) (Shared String) (Shared ([a],[b],Bool)) !*IWorld -> (MaybeError TaskException (), *IWorld) | iTask a & iTask b
serialDeviceBackgroundTask dp enc dec accShare rw iworld
	= case read rw iworld of
		(Error e, iworld) = (Error $ exception "share couldn't be read", iworld)

		//We need to stop
		(Ok (_,_,True), iworld) = (Ok (), iworld)

		(Ok (r,s,ss), iworld)
		# (merr, iworld) = read accShare iworld
		| isError merr = (liftError merr, iworld)
		# (Ok acc) = merr
		# (mtty, iworld) = getTTYResource dp iworld
		= case mtty of
			Nothing = (Error (exception "The tty device is gone"), iworld)
			Just (tty, bgid)
			# tty = foldr TTYwrite tty $ reverse $ map enc s
			# (newdata, tty) = readWhileAvailable tty
			# iworld = {iworld & resources=[TTYd (dp, bgid, tty):iworld.resources]}
			= case dec (acc +++ newdata) of
				(Left err, newacc) = (Error (exception "Error while parsing"), iworld)
				(Right msgs, newacc)
					# (merr, iworld) = if (msgs =: [])
						if (s =: [])
							(Ok (), iworld)
							(write (r, [], False) rw iworld)
						(write (r++msgs, [], False) rw iworld)
					| isError merr = (liftError merr, iworld)
					= write newacc accShare iworld

readWhileAvailable :: !*TTY -> (String, !*TTY)
readWhileAvailable tty
# (available, tty) = TTYavailable tty
| not available = ("", tty)
# (c, tty) = TTYread tty
# (cs, tty) = readWhileAvailable tty
= (toString (toChar c) +++ cs, tty)
