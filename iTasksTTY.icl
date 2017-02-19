implementation module iTasksTTY

import TTY

import StdList
from StdFunc import o, flip
import StdMisc
import StdString

import System.Directory
import iTasks
from Data.Func import $
from Text import class Text(startsWith), instance Text String

import iTasks.UI.Definition

import iTasks._Framework.TaskState
import iTasks._Framework.TaskServer
import iTasks._Framework.IWorld
import iTasks._Framework.Store

:: *Resource | TTYd !*TTY

derive class iTask TTYSettings, Parity, BaudRate, ByteSize

getTTYDevices :: !*env -> *(![String], !*env)
getTTYDevices w = case readDirectory "/dev" w of
	(Error (errcode, errmsg), w) = abort errmsg
	(Ok entries, w) = (map ((+++) "/dev/") (filter isTTY entries), w)
	where
		isTTY s = not (isEmpty (filter (flip startsWith s) prefixes))
		prefixes = ["ttyS", "ttyACM", "ttyUSB", "tty.usbserial"]

syncSerialChannel :: String TTYSettings (b -> String) (String -> a) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
syncSerialChannel dev opts enc dec rw = Task eval
	where
		eval event evalOpts tree=:(TCInit taskId ts) iworld=:{IWorld|world}
		= case TTYopen dev opts world of
			(False, _, world)
			# (err, world) = TTYerror world
			= (ExceptionResult (exception err), {iworld & world=world})
			(True, tty, world)
			# iworld = {iworld & world=world, resources=Just (TTYd tty)}
			= case addBackgroundTask 42 (BackgroundTask (serialDeviceBackgroundTask rw)) iworld of
				(Error e, iworld) = (ExceptionResult (exception "h"), iworld)
				(Ok _, iworld) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange (TCBasic taskId ts JSONNull False), iworld)

		eval _ _ tree=:(TCBasic _ ts _ _) iworld
		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} NoChange tree, iworld)

		eval event evalOpts tree=:(TCDestroy _) iworld=:{IWorld|resources,world}
		# (TTYd tty) = fromJust resources
		# (ok, world) = TTYclose tty world
		# iworld = {iworld & world=world,resources=Nothing}
		= case removeBackgroundTask 42 iworld of
			(Error e, iworld) = (ExceptionResult (exception "h"), iworld)
			(Ok _, iworld) = (DestroyedResult, iworld)

//		serialDeviceBackgroundTask :: (Shared ([MTaskMSGRecv],[MTaskMSGSend],Bool)) !*IWorld -> (MaybeError TaskException (), *IWorld)
		serialDeviceBackgroundTask rw iworld
			= case read rw iworld of
				(Error e, iworld) = (Error $ exception "share couldn't be read", iworld)
				(Ok (r,s,ss), iworld)
				# (Just (TTYd tty)) = iworld.resources
				# tty = writet (map enc s) tty
				# (ml, tty) = case TTYavailable tty of
					(False, tty) = ([], tty)
					(_, tty)
					# (l, tty) = TTYreadline tty
					= ([dec l], tty)
				# iworld = {iworld & resources=Just (TTYd tty)}
				= case write (r++ml,[],False) rw iworld of
					(Error e, iworld) = (Error $ exception "share couldn't be written", iworld)
					(Ok _, iworld) = case notify rw iworld of
						(Error e, iworld) = (Error $ exception "share couldn't be notified", iworld)
						(Ok _, iworld) = (Ok (), iworld)
			where
				writet :: [String] -> (*TTY -> *TTY)
				writet [] = id
				writet [x:xs] = writet xs o TTYwrite x
