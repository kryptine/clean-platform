definition module CleanPrettyPrint

/**
 * Pretty-printer for types in the Clean compiler
 */

from syntax import
	:: AType,
	:: ParsedDefinition,
	:: ParsedExpr,
	:: Rhs,
	:: Type,
	:: TypeContext

/**
 * Pretty-printer
 *
 * @var The type to print
 * @param The value to print
 * @result A string representation of the parameter
 */
class cpp t :: t -> String

instance cpp
	AType,
	ParsedDefinition,
	ParsedExpr,
	Rhs,
	Type,
	TypeContext
