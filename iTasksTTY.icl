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

import iTasks._Framework.TaskState
import iTasks._Framework.TaskServer
import iTasks._Framework.IWorld
import iTasks._Framework.Store

:: *Resource | TTYd *[*(String,Int,!*TTY)]

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
			= case addBackgroundTask (BackgroundTask (serialDeviceBackgroundTask opts.devicePath enc dec rw)) iworld of
				(Error e, iworld) = (ExceptionResult (exception "h"), iworld)
				(Ok bgid, iworld) = case iworld.resources of
					Nothing = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep (TCBasic taskId ts JSONNull False),
						{iworld & resources=Just $ TTYd [(opts.devicePath, bgid, tty)]})
					Just (TTYd m) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep (TCBasic taskId ts JSONNull False),
						{iworld & resources=Just $ TTYd [(opts.devicePath, bgid, tty):m]})

		eval _ _ tree=:(TCBasic _ ts _ _) iworld
		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} rep tree, iworld)

		eval event evalOpts tree=:(TCDestroy _) iworld=:{IWorld|resources,world}
		# (Just (TTYd ttys)) = resources
		# ((tty, bgid), ttys) = flt opts.devicePath ttys
		# (ok, world) = TTYclose tty world
		# iworld = {iworld & world=world,resources=Just $ TTYd ttys}
		= case removeBackgroundTask bgid iworld of
			(Error e, iworld) = (ExceptionResult (exception "h"), iworld)
			(Ok _, iworld) = (DestroyedResult, iworld)

		rep = ReplaceUI $ stringDisplay $ "Serial client " <+++ opts.devicePath

flt :: String [(String, Int, *TTY)] -> ((*TTY, Int), [(String, Int, *TTY)])
flt m [] = abort "not found"
flt m [(p,b,t):ps]
| p == m = ((t, b), ps)
# (tty, pps) = flt m ps 
= (tty, [(p,b,t):pps])

import StdDebug, StdMisc
serialDeviceBackgroundTask :: String (b -> String) (String -> a) (Shared ([a],[b],Bool)) !*IWorld -> (MaybeError TaskException (), *IWorld) | iTask a & iTask b
serialDeviceBackgroundTask dp enc dec rw iworld
	= case read rw iworld of
		(Error e, iworld) = (Error $ exception "share couldn't be read", iworld)
		//We need to stop
		(Ok (_,_,True), iworld) = (Ok (), iworld)

		(Ok (r,s,ss), iworld)
		# (Just (TTYd ttys)) = iworld.resources
		# ((tty, bgid), ttys) = flt dp ttys
		# tty = foldr TTYwrite tty $ reverse $ map enc s
		# (ml, tty) = case TTYavailable tty of
			(False, tty) = ([], tty)
			(_, tty) = appFst (pure o dec) $ TTYreadline tty
		# iworld = {iworld & resources=Just (TTYd [(dp, bgid, tty):ttys])}
		| isEmpty ml && isEmpty s = (Ok (), iworld)
		# (merr, iworld) = notify rw iworld
		| isError merr = (Error $ fromError merr, iworld)
		= write (r++ml,[],False) rw iworld
