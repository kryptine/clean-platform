implementation module System.Options

import StdClass
from StdFunc import flip, o
import StdList
import StdString

import Data.Error
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
from Text import class Text(join), instance Text String

instance toString HelpText
where
	// TODO: make this prettier and dependent on the rest of the help text
	toString (OptionHelpText opts args help additional) = join " "
		[ join "/" opts
		, join " " args
		, join "\n  " [help:additional]
		]

parseOptions :: (t opts) [String] opts -> MaybeError [String] opts | OptionDescription t
parseOptions p args opts = parse (optParser p) args opts
where
	parse (OptParser p) []   opts = Ok opts
	parse (OptParser p) args opts = case p args opts of
		Nothing               -> Error ["Unknown option '" +++ hd args +++ "'"]
		Just (Error es)       -> Error es
		Just (Ok (opts,rest)) -> parse (OptParser p) rest opts

instance OptionDescription Option
where
	optParser :: (Option opts) -> OptParser opts
	optParser (Flag f upd _) = OptParser \args opts -> case args of
		[arg:args] -> if (arg <> f)
			Nothing
			(Just $ flip tuple args <$> upd opts)
		[] -> Nothing
	optParser (Option opt upd _ _) = OptParser \args opts -> case args of
		[arg:args] -> if (arg <> opt)
			Nothing
			(case args of
				[arg:args] -> Just $ flip tuple args <$> upd arg opts
				[]         -> Just $ Error ["'" +++ opt +++ "' requires an argument"])
		[] -> Nothing
	optParser (Shorthand short long child) = OptParser \args opts ->
		let (OptParser p`) = optParser child in
		p` (map (\arg -> if (arg == short) long arg) args) opts
	optParser (Options ps) = OptParser \args opts -> case catMaybes [p args opts \\ p <- optps] of
		[]      -> Nothing
		[res:_] -> Just res
	where
		optps = [p \\ OptParser p <- map optParser ps]
	optParser wh=:(WithHelp p) = OptParser \args opts -> case args of
		["--help":args] -> Just (Error $ map toString $ helpText wh)
		_               -> let (OptParser p`) = optParser p in p` args opts

	helpText (Flag a _ h) = [OptionHelpText [a] [] h []]
	helpText (Option o _ n h) = [OptionHelpText [o] [n] h []]
	helpText (Shorthand short long child) = map upd (helpText child)
	where
		upd :: HelpText -> HelpText
		upd oht=:(OptionHelpText opts args help add)
		| isMember long opts = OptionHelpText (opts ++ [short]) args help add
		| otherwise          = oht
	helpText (Options ps) = concatMap helpText ps
	helpText (WithHelp p) =
		[ OptionHelpText ["--help"] [] "Show this help text" []
		: helpText p
		]

WithShortHelp :: ((t opts) -> Option opts) | OptionDescription t
WithShortHelp = Shorthand "-h" "--help" o WithHelp
