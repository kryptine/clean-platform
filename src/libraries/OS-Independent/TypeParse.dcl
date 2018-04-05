definition module TypeParse

/**
 * A parser for Clean types.
 */

from Data.Maybe import :: Maybe
from TypeDef import :: Type

/**
 * Parse a Clean type.
 */
parseType :: [Char] -> Maybe Type
