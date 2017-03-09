definition module TypeUnify

import TypeDef

from Data.Maybe import ::Maybe

prepare_unification :: !Bool /* is left */ [TypeDef] !Type -> ([TypeDef], Type)
finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier
unify :: ![Instance] !Type !Type -> Maybe [TVAssignment]
