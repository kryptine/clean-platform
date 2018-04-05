definition module CleanPrettyPrint.Expression

from CleanPrettyPrint.Util import class print

from syntax import :: ParsedExpr, :: Rhs, :: OptGuardedAlts

instance print ParsedExpr, Rhs

/**
 * `True` iff the right-hand side is a {{`GuardedAlts`}} or {{`UnguardedExpr`}}
 * with at least one {{`ewl_node`}}.
 */
compound_rhs :: OptGuardedAlts -> Bool
