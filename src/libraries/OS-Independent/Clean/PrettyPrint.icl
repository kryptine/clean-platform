implementation module CleanPrettyPrint

import StdEnv

import syntax

import CleanPrettyPrint.Util
import CleanPrettyPrint.Common
import CleanPrettyPrint.Expression
import CleanPrettyPrint.Definition

instance cpp AType            where cpp x = print zero x; cppp x = print zerop x
instance cpp ParsedDefinition where cpp x = print zero x; cppp x = print zerop x
instance cpp ParsedExpr       where cpp x = print zero x; cppp x = print zerop x
instance cpp Rhs              where cpp x = print zero x; cppp x = print zerop x
instance cpp Type             where cpp x = print zero x; cppp x = print zerop x
instance cpp TypeContext      where cpp x = print zero x; cppp x = print zerop x

zerop = {zero & cpp_parens=True}
