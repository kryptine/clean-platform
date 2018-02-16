definition module Data.Generics.GenDiff

import StdGeneric

/**
 * The difference between two values.
 */
:: Diff =
	{ status   :: !DiffStatus //* Whether this node is common, added or removed.
	, value    :: !String     //* A String representation of the value.
	, children :: ![Diff]     //* Diffs on the childrens of this node.
	}

/**
 * The status of a node head in a {{`Diff`}}.
 */
:: DiffStatus
	= Common  //* The complete node is common to both values
	| Changed //* The node head is common, but there are diffs in the children
	| Added   //* The node is added
	| Removed //* The node is removed

/**
 * Compute the {{`Diff`}} between two values.
 */
generic gDiff a :: a a -> [Diff]
derive gDiff Int, UNIT, PAIR, EITHER, OBJECT, CONS of d
derive gDiff []

/**
 * A String representation of a {{`Diff`}} using ANSI escape codes.
 */
diffToConsole :: [Diff] -> String
