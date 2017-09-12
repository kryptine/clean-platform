implementation module iTasksTTY

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

getTTYDevices :: !*env -> *(![String], !*env)
getTTYDevices w = case readDirectory "/dev" w of
	(Error (errcode, errmsg), w) = abort errmsg
	(Ok entries, w) = (map ((+++) "/dev/") (filter isTTY entries), w)
	where
		isTTY s = not (isEmpty (filter (flip startsWith s) prefixes))
		prefixes = ["tty", "rfcomm"]

enterTTYSettings :: Task TTYSettings
enterTTYSettings = accWorld getTTYDevices
	>>= \ds->(
				enterChoice "Device" [] ds 
			-&&- updateInformation "Baudrate" [] B9600
			<<@ ArrangeHorizontal)
		-&&- (
				updateInformation "Bytesize" [] BytesizeEight
			-&&- updateInformation "Parity" [] ParityNone
			<<@ ArrangeHorizontal)
		-&&- (
				updateInformation "Stop2bits" [] False
			-&&- updateInformation "Xonoff" [] False
			<<@ ArrangeHorizontal)
	@ \((dev, br), ((bs, pr), (st, xo)))->makeTTYSettings dev br bs pr st xo

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

syncSerialChannel :: TTYSettings (b -> String) (String -> a) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
syncSerialChannel opts enc dec rw = Task eval
	where
		eval event evalOpts tree=:(TCInit taskId ts) iworld
		# (mtty, iworld) = getTTYResource opts.devicePath iworld
		= case mtty of
			Just (tty, _) = (ExceptionResult (exception "This tty was already open"), iworld)
			Nothing = case TTYopen opts iworld.world of
				(False, _, world)
				# (err, world) = TTYerror world
				= (ExceptionResult (exception err), {iworld & world=world})
				(True, tty, world)
				# iworld = {iworld & world=world}
				= case addBackgroundTask (BackgroundTask (serialDeviceBackgroundTask opts.devicePath enc dec rw)) iworld of
					(Error e, iworld) = (ExceptionResult (exception "background task couldn't be added"), iworld)
					(Ok bgid, iworld) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep (TCBasic taskId ts JSONNull False),
						{iworld & resources=[TTYd (opts.devicePath, bgid, tty):iworld.resources]})

		eval _ _ tree=:(TCBasic _ ts _ _) iworld
		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} rep tree, iworld)

		eval event evalOpts tree=:(TCDestroy _) iworld=:{IWorld|resources,world}
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

import StdDebug, StdMisc
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
serialDeviceBackgroundTask :: String (b -> String) (String -> a) (Shared ([a],[b],Bool)) !*IWorld -> (MaybeError TaskException (), *IWorld) | iTask a & iTask b
serialDeviceBackgroundTask dp enc dec rw iworld
	= case read rw iworld of
		(Error e, iworld) = (Error $ exception "share couldn't be read", iworld)

		//We need to stop
		(Ok (_,_,True), iworld) = (Ok (), iworld)

		(Ok (r,s,ss), iworld)
		# (mtty, iworld) = getTTYResource dp iworld
		= case mtty of
			Nothing = (Error (exception "The tty device is gone"), iworld)
			Just (tty, bgid)
			# tty = foldr TTYwrite tty $ reverse $ map enc s
			# (ml, tty) = case TTYavailable tty of
				(False, tty) = ([], tty)
				(_, tty) = appFst (pure o dec) $ TTYreadline tty
			# iworld = {iworld & resources=[TTYd (dp, bgid, tty):iworld.resources]}
			| isEmpty ml && isEmpty s = (Ok (), iworld)
			# (merr, iworld) = notify rw iworld
			| isError merr = (Error $ fromError merr, iworld)
			= write (r++ml,[],False) rw iworld
