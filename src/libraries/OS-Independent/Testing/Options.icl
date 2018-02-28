implementation module Testing.Options

import StdList

import Data.Error
from Data.Func import $
import Data.Generics.GenDefault
import Data.Maybe
import System.Options

gDefault{|Bool|} = False
derive gDefault TestOptions, TestRun

testOptionDescription :: Option TestOptions
testOptionDescription = WithShortHelp $ Options
	[ Shorthand "-l" "--list" $ Flag
		"--list"
		(\opts -> Ok {opts & list=True})
		"List all available tests"
	, Shorthand "-O" "--option" $ Option
		"--option"
		(\opt opts -> case opts.runs of
			[]     -> Error ["--option used before --run"]
			[r:rs] -> Ok {opts & runs=[{r & options=r.options ++ [opt]}:rs]})
		"OPT"
		"Add OPT to the options of the previously added test"
	, Shorthand "-r" "--run"  $ Option
		"--run"
		(\r opts -> Ok {opts & runs=opts.runs ++ [{name=r, options=[]}]})
		"NAME"
		"Run test NAME"
	, Shorthand "-s" "--skip" $ Option
		"--skip"
		(\r opts -> Ok {opts & skip=[r:opts.skip]})
		"NAME"
		"Skip test NAME"
	]

Start = parseOptions testOptionDescription ["-h"] gDefault{|*|}
