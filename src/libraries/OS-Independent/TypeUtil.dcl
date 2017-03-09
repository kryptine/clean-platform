definition module TypeUtil

import TypeDef

from StdFunc import flip
from StdOverloaded import class toString (toString)

from Control.Monad import class Applicative, class Monad, foldM
from Data.Functor import class Functor
from Data.Maybe import ::Maybe

class print a :: Bool a -> [String] // isArg x -> string representation

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

propagate_uniqueness :: Type -> Type
resolve_synonyms :: [TypeDef] Type -> ([TypeDef], Type)

assign :: !TVAssignment !Type -> Maybe Type
assignAll :== flip (foldM (flip assign)) // ([TVAssignment] Type -> Maybe Type)
