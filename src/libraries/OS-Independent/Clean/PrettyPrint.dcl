definition module CleanPrettyPrint

from syntax import
	:: AType,
	:: ParsedDefinition,
	:: ParsedExpr,
	:: Rhs,
	:: Type,
	:: TypeContext

class cpp t :: t -> String

instance cpp
	AType,
	ParsedDefinition,
	ParsedExpr,
	Rhs,
	Type,
	TypeContext
