definition module commentstest

/**
 * This module is used to test the Clean documentation parser in Clean.Parse.Comments.
 * The documentation here is written obscurely on purpose!
 */

import syntax
import Clean.Parse.Comments

//* A documentation entry
:: Entry =
	{ kind  :: !String //* the kind of thing that is documented
	, name  :: !String
		//* the name of the documented thing
	, value :: !Maybe String
	}

//* This type is just here to test; it isn't used
:: TrickyADT
	= TrickyADT_A //* Documentation on same line
	| TrickyADT_B
		//* Documentation on new line
		//* Extra documentation line
	| TrickyADT_C
		//* Documentation on new line

list_comments :: !ParsedModule !CollectedComments -> [Entry]
// Don't add a comment for this function: we want to check that the last comment
// of TrickyADT is not added to list_comments_of_definitions.
