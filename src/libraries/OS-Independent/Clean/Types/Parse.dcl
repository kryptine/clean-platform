definition module Clean.Types.Parse

/**
 * A parser for Clean types.
 */

from Data.Maybe import :: Maybe
from Clean.Types import :: Type

/**
 * Parse a Clean type.
 */
parseType :: ![Char] -> Maybe Type
