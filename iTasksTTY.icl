implementation module iTasksTTY

import TTY

import StdList
from StdFunc import o, flip
import StdMisc
import StdString
import Data.List

import System.Directory
import iTasks
import Data.Tuple
import Control.Applicative
from Data.Func import $
from Text import class Text(startsWith), instance Text String

import iTasks.UI.Definition

import iTasks._Framework.TaskState
import iTasks._Framework.TaskServer
import iTasks._Framework.IWorld
import iTasks._Framework.Store

:: *Resource | TTYd !*TTY Int

derive class iTask TTYSettings, Parity, BaudRate, ByteSize

getTTYDevices :: !*env -> *(![String], !*env)
getTTYDevices w = case readDirectory "/dev" w of
	(Error (errcode, errmsg), w) = abort errmsg
	(Ok entries, w) = (map ((+++) "/dev/") (filter isTTY entries), w)
	where
		isTTY s = not (isEmpty (filter (flip startsWith s) prefixes))
		prefixes = ["ttyS", "ttyACM", "ttyUSB", "tty.usbserial"]

enterTTYSettings :: Task TTYSettings
enterTTYSettings = accWorld getTTYDevices
	>>= \ds->(((((enterChoice "Device" [] ds
	-&&- updateInformation "Baudrate" [] B9600)
	-&&- updateInformation "Bytesize" [] BytesizeEight)
	-&&- updateInformation "Parity" [] ParityNone)
	-&&- updateInformation "Stop2bits" [] False)
	-&&- updateInformation "Xonoff" [] False)
	@ (uncurry o uncurry o uncurry o uncurry o uncurry) makeTTYSettings

syncSerialChannel :: TTYSettings (b -> String) (String -> a) (Shared ([a],[b],Bool)) -> Task () | iTask a & iTask b
syncSerialChannel opts enc dec rw = Task eval
	where
		eval event evalOpts tree=:(TCInit taskId ts) iworld=:{IWorld|world,ioTasks={todo}}
		= case TTYopen opts world of
			(False, _, world)
			# (err, world) = TTYerror world
			= (ExceptionResult (exception err), {iworld & world=world})
			(True, tty, world)
			# iworld = {iworld & world=world}
			= case addBackgroundTask (BackgroundTask (serialDeviceBackgroundTask enc dec rw)) iworld of
				(Error e, iworld) = (ExceptionResult (exception "h"), iworld)
				(Ok bgid, iworld) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep (TCBasic taskId ts JSONNull False), {iworld & resources=Just (TTYd tty bgid)})

		eval _ _ tree=:(TCBasic _ ts _ _) iworld
		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} rep tree, iworld)

		eval event evalOpts tree=:(TCDestroy _) iworld=:{IWorld|resources,world}
		# (TTYd tty bgid) = fromJust resources
		# (ok, world) = TTYclose tty world
		# iworld = {iworld & world=world,resources=Nothing}
		= case removeBackgroundTask bgid iworld of
			(Error e, iworld) = (ExceptionResult (exception "h"), iworld)
			(Ok _, iworld) = (DestroyedResult, iworld)

		rep = ReplaceUI $ stringDisplay $ "Serial client " <+++ opts.devicePath

serialDeviceBackgroundTask :: (b -> String) (String -> a) (Shared ([a],[b],Bool)) !*IWorld -> (MaybeError TaskException (), *IWorld) | iTask a & iTask b
serialDeviceBackgroundTask enc dec rw iworld
	= case read rw iworld of
		(Error e, iworld) = (Error $ exception "share couldn't be read", iworld)
		//We need to stop
		(Ok (_,_,True), iworld) = (Error $ exception "I have to stop...", iworld)
		(Ok (r,s,ss), iworld)
		# (Just (TTYd tty bgid)) = iworld.resources
		# tty = writet (map enc s) tty
		# (ml, tty) = case TTYavailable tty of
			(False, tty) = ([], tty)
			(_, tty) = appFst (pure o dec) $ TTYreadline tty
		# iworld = {iworld & resources=Just (TTYd tty bgid)}
		| isEmpty ml = (Ok (), iworld)
		= case write (r++ml,[],False) rw iworld of
			(Error e, iworld) = (Error $ exception "share couldn't be written", iworld)
			(Ok _, iworld) = (Ok (), iworld)
	where
		writet :: [String] -> (*TTY -> *TTY)
		writet [] = id
		writet [x:xs] = writet xs o TTYwrite x
