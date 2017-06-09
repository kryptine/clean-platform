definition module TypeUnify

/**
 * Functions to unify Clean types
 */

import TypeDef

from Data.Maybe import ::Maybe

/**
 * Prepare a type for unification. Unification always happens between a 'left'
 * and a 'right' type. Unification of two left or two right types may yield
 * incorrect results.
 *
 * @param True if this is the left type
 * @param A list of all known type definitions to use for resolving synonyms
 * @param The type to prepare
 * @result The type synonyms used and the type after preparation
 */
prepare_unification :: !Bool [TypeDef] !Type -> ([TypeDef], Type)

/**
 * Finish unification, yielding a unifier
 *
 * @param The used type synonyms
 * @param The variable assignments
 * @result The unifier
 */
finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier

/**
 * Core of the unification. An implementation of Martelli and Montanari, 'An
 * Efficient Unification Algorithm'. ACM Transactions on Programming Languages
 * and Systems, Vol. 4, No. 2, April 1982, Pages 258-282.
 * It has been modified slightly to deal with constructor variables, universal
 * quantifiers and uniqueness.
 *
 * @param Instances, to match class contexts. Currently ignored.
 * @param The left type
 * @param The right type
 * @result A list of type variable assignments, or Nothing if unification failed
 */
unify :: ![Instance] !Type !Type -> Maybe [TVAssignment]
