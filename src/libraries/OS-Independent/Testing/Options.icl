implementation module Testing.Options

import StdString

import Control.Monad
import Data.Error
from Data.Func import $
import Data.Generics.GenDefault
import Data.Generics.GenEq
import Data.List
import Data.Maybe
import System.FilePath
import Testing.TestEvents
from Text import class Text(split), instance Text String

derive gEq MessageType; instance == MessageType where == a b = a === b

gDefault{|Bool|} = False
derive gDefault MessageType, Options, OutputFormat, Run, Strategy

messageType :: TestEvent -> MessageType
messageType (StartEvent _) = MT_Started
messageType (EndEvent ee) = case ee.event of
	Passed  -> MT_Passed
	Failed  -> MT_Failed
	Skipped -> MT_Skipped

LONG_OPTIONS =:
	[ ("-f", "--output-format")
	, ("-h", "--help")
	, ("-H", "--hide")
	, ("-n", "--name")
	, ("-O", "--option")
	, ("-r", "--run")
	, ("-s", "--skip")
	, ("-S", "--strategy")
	]

parseTestOpts :: Options [String] -> MaybeErrorString Options
parseTestOpts opts [] = Ok {opts & runs=reverse opts.runs}
parseTestOpts opts [arg:args] | isJust opt = parseTestOpts opts [fromJust opt:args]
where opt = lookup arg LONG_OPTIONS
parseTestOpts opts ["--help":args] = parseTestOpts {opts & help=True} args
parseTestOpts opts ["--hide":args] = case args of
	[arg:args] -> mapM parseMT (split "," arg) >>= \h -> parseTestOpts {opts & hide=h} args
	[]         -> Error "--hide requires a parameter"
where
	parseMT :: String -> MaybeErrorString MessageType
	parseMT "start" = Ok MT_Started
	parseMT "pass"  = Ok MT_Passed
	parseMT "fail"  = Ok MT_Failed
	parseMT "skip"  = Ok MT_Skipped
	parseMT "lost"  = Ok MT_Lost
	parseMT s       = Error $ "Unknown message type '" +++ s +++ "'"
parseTestOpts opts ["--name":args] = case args of
	[name:args] -> case opts.runs of
		[]      -> Error "-n used before -r"
		[r:rs]  -> parseTestOpts {opts & runs=[{Run | r & name=name}:rs]} args
	[]          -> Error "-n requires a parameter"
parseTestOpts opts ["--option":args] = case args of
	[opt:args] -> case opts.runs of
		[]     -> Error "--option used before --run"
		[r:rs] -> parseTestOpts {opts & runs=[{r & options=r.options ++ [opt]}:rs]} args
	[]         -> Error "--option requires a parameter"
parseTestOpts opts ["--output-format":args] = case args of
	["json":args]  -> parseTestOpts {opts & output=OF_JSON} args
	["human":args] -> parseTestOpts {opts & output=OF_HumanReadable} args
	[fmt:args]     -> Error $ "Unknown output format '" +++ fmt +++ "'"
	[]             -> Error "--output-format requires a parameter"
parseTestOpts opts ["--run":args] = case args of
	[exe:args] -> parseTestOpts {opts & runs=[{gDefault{|*|} & executable=exe, name=exe}:opts.runs]} args
	[]         -> Error "--run requires a parameter"
parseTestOpts opts ["--skip":args] = case args of
	[name:args] -> parseTestOpts {opts & skip=opts.skip ++ [name]} args
	[]          -> Error "--skip requires a parameter"
parseTestOpts opts ["--strategy":args] = case args of
	["default":args]      -> parseTestOpts {opts & strategy=S_Default} args
	["failed-first":args] -> parseTestOpts {opts & strategy=S_FailedFirst} args
	[s:args]              -> Error $ "Unknown strategy '" +++ s +++ "'"
	[]                    -> Error "--strategy requires a parameter"
parseTestOpts opts [arg:args] = Error $ "Unknown option '" +++ arg +++ "'"

optionDoc :: [String]
optionDoc =
	[ "General options:"
	, "  --help/-h               Show this help"
	, "  --hide/-H TYPE          Comma-separated list of types of messages to hide (start,pass,fail,skip,lost)"
	, "  --output-format/-f FMT  The output format (json,human)"
	, "  --skip/-S NAME          Skip tests with this name"
	, "  --strategy/-S STRATEGY  The test order strategy, where STRATEGY is one of"
	, "      default               Order of the --run parameters"
	, "      failed-first          First run the tests that failed last time; if they past continue with the rest"
	, "Test options:"
	, "  --run/-r EXE            Execute tests from executable EXE"
	, "  --option/-O OPT         Add OPT to the command line of the previously added run"
	, "  --name/-n NAME          Give the previously added run the name NAME"
	]
