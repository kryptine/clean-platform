definition module Testing.Options

from Data.Generics.GenDefault import generic gDefault
from System.Options import :: Option

:: TestOptions =
	{ runs     :: ![TestRun]
	, skip     :: ![String]
	, list     :: !Bool
	}

:: TestRun =
	{ name       :: !String
	, options    :: ![String]
	}

derive gDefault TestOptions, TestRun

testOptionDescription :: Option TestOptions
