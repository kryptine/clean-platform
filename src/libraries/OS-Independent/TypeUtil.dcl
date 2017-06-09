definition module TypeUtil

/**
 * Utility functions for Clean types
 */

import TypeDef

from StdFunc import flip
from StdOverloaded import class toString (toString)

from Control.Monad import class Applicative, class Monad, foldM
from Data.Functor import class Functor
from Data.Maybe import ::Maybe

/**
 * Pretty printer
 *
 * @var The type to print
 * @param True iff parentheses should be placed around compound elements
 * @param The element to print
 * @result A list of strings that should be concatenated
 */
class print a :: Bool a -> [String]

instance print String
instance print Int

instance print [a] | print a
instance print (Maybe a) | print a

instance print ClassOrGeneric
instance print ClassRestriction
instance print ClassContext
instance print Type
instance print TypeDef
instance print Priority

/**
 * Propagate uniqueness up, as described in section 9.2 of the Clean language
 * report.
 */
propagate_uniqueness :: Type -> Type

/**
 * Resolve all synonyms in a type
 *
 * @param The type synonyms to use
 * @param The type to resolve
 * @param The used synonyms and the new type
 */
resolve_synonyms :: [TypeDef] Type -> ([TypeDef], Type)

/**
 * Apply a variable assignment on a type, if possible
 */
assign :: !TVAssignment !Type -> Maybe Type

/**
 * Apply a list of variable assignments on a type
 *
 * @type [TVAssignment] Type -> Maybe Type
 */
assignAll :== flip (foldM (flip assign))
