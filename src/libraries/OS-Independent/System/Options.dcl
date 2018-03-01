definition module System.Options

from StdOverloaded import class toString

from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe

/**
 * A parser for command line options.
 * The arguments are the list of command line arguments and the current
 * settings object. The result is:
 *  - `Nothing`, if the parser cannot be applied
 *  - `Just (Error es)`, if the parser finished with an error, where `es` is a
 *    list of errors/warning.
 *  - `Just (Ok (opts,args))`, if the parser succeeded, where `opts` is the
 *    new settings object and `args` the list of the rest of the arguments.
 *
 * @var The settings object type
 */
:: OptParser opts
	= OptParser ([String] opts -> Maybe (MaybeError [String] (opts, [String])))

/**
 * An element in the help text of a command line application.
 */
:: HelpText
	= OptionHelpText [String] [String] String [String]
		//* Help text for an option: the option variants, the meta variables, a description, additional lines

instance toString HelpText

/**
 * Types instantiating this class can be combined to form new parsers.
 */
class OptionDescription t
where
	/**
	 * Create a parser from an option description.
	 * @param The option description
	 * @result The corresponding parser
	 */
	optParser :: (t opts) -> OptParser opts

	/**
	 * The help text belonging to an option description.
	 * @param The option description
	 * @result The corresponding parser
	 */
	helpText :: (t opts) -> [HelpText]

/**
 * Parse commnd line arguments.
 *
 * @param The option descriptions
 * @param The command line arguments (see {{`getCommandLine`}})
 * @param The default settings
 * @result Either a list of error/warning messages or the new settings object
 */
parseOptions :: (t opts) [String] opts -> MaybeError [String] opts | OptionDescription t

/**
 * Basic command line options
 */
:: Option opts
	= Flag String (opts -> MaybeError [String] opts) String
		//* A flag is a command line option without arguments: flag, update function, help text
	| Option String (String opts -> MaybeError [String] opts) String String
		//* An option is a command line option with one argument: option, update function, meta variable, help text
	| E.t: Shorthand String String (t opts) & OptionDescription t
		//* A shorthand translates a short option into a long one: short version, long version, child parser
	| E.t: Options [t opts] & OptionDescription t
		//* A collections of option descriptions
	| E.t: WithHelp (t opts) & OptionDescription t
		//* Adds a --help option to display the help text

/**
 * Like {{`WithHelp`}} combined with `{{Shorthand}} "-h" "--help"`
 */
WithShortHelp :: ((t opts) -> Option opts) | OptionDescription t

instance OptionDescription Option
