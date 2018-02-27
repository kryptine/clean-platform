definition module Testing.Options

from StdOverloaded import class ==

from Data.Error import :: MaybeError, :: MaybeErrorString
from Data.Generics.GenDefault import generic gDefault
from System.FilePath import :: FilePath
from Testing.TestEvents import :: TestEvent

:: Options =
	{ runs     :: ![Run]
	, skip     :: ![String]
	, help     :: !Bool
	, list     :: !Bool
	, output   :: !OutputFormat
	, hide     :: ![MessageType]
	, strategy :: !Strategy
	}

:: MessageType = MT_Started | MT_Passed | MT_Failed | MT_Skipped | MT_Lost
:: OutputFormat = OF_JSON | OF_HumanReadable
:: Strategy = S_Default | S_FailedFirst

:: Run =
	{ name       :: !String
	, executable :: !FilePath
	, options    :: ![String]
	}

instance == MessageType

derive gDefault MessageType, Options, OutputFormat, Run, Strategy

messageType :: TestEvent -> MessageType

parseTestOpts :: Options [String] -> MaybeErrorString Options

optionDoc :: [String]
