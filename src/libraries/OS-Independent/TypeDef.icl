implementation module TypeDef

from StdOverloaded import class ==(..), class length(..)
from StdClass import class Eq
import StdList
import StdTuple
from StdString import instance == {#Char}
import StdBool
from StdFunc import o, id
from GenEq import generic gEq, ===
from Data.Func import $
import Data.Maybe

derive gEq Maybe, Type, TypeRestriction, Kind

instance == Type where == a b = a === b
instance == TypeRestriction where == a b = a === b

subtypes :: Type -> [Type]
subtypes t=:(Type s ts) = removeDup [t : flatten (map subtypes ts)]
subtypes t=:(Func is r tc) = removeDup [t : flatten (map subtypes [r:is])]
subtypes t=:(Cons c ts) = removeDup [t : flatten (map subtypes ts)]
subtypes t=:(Uniq t`) = removeDup [t : subtypes t`]
subtypes t=:(Forall vs t` tc) = removeDup [t : flatten (map subtypes [t`:vs])]
subtypes t=:(Var _) = [t]
subtypes t=:(Arrow mt) = [t:flatten (map subtypes (maybeToList mt))]

allVars :: (Type -> [TypeVar])
allVars = removeDup o map name o filter (\t -> isCons t || isVar t) o subtypes
where
	name :: Type -> TypeVar
	name (Cons v _) = v
	name (Var v) = v

allUniversalVars :: Type -> [TypeVar]
allUniversalVars (Forall vs t tc) = removeDup (flatten (map allVars vs) ++ allUniversalVars t)
allUniversalVars (Type _ ts) = removeDup (flatten (map allUniversalVars ts))
allUniversalVars (Func is r _) = removeDup (flatten (map allUniversalVars [r:is]))
allUniversalVars (Cons _ ts) = removeDup (flatten (map allUniversalVars ts))
allUniversalVars (Uniq t) = allUniversalVars t
allUniversalVars (Var _) = []
allUniversalVars (Arrow (Just t)) = allUniversalVars t
allUniversalVars (Arrow Nothing)  = []

isVar :: Type -> Bool
isVar (Var _) = True; isVar _ = False

fromVar :: Type -> TypeVar
fromVar (Var v) = v

fromVarLenient :: Type -> TypeVar
fromVarLenient (Var v) = v
fromVarLenient (Cons v _) = v
fromVarLenient (Uniq t) = fromVarLenient t

isCons :: Type -> Bool
isCons (Cons _ _) = True; isCons _ = False

isCons` :: TypeVar Type -> Bool
isCons` v (Cons v` _) = v == v`; isCons` _ _ = False

isVarOrCons` :: TypeVar Type -> Bool
isVarOrCons` v (Var v`)    = v == v`
isVarOrCons` v (Cons v` _) = v == v`
isVarOrCons` _ _           = False

isType :: Type -> Bool
isType (Type _ _) = True; isType _ = False

isFunc :: Type -> Bool
isFunc (Func _ _ _) = True; isFunc _ = False

isUniq :: Type -> Bool
isUniq (Uniq _) = True; isUniq _ = False

isForall :: Type -> Bool
isForall (Forall _ _ _) = True; isForall _ = False

fromForall :: Type -> Type
fromForall (Forall _ t _) = t

isArrow :: Type -> Bool
isArrow (Arrow _) = True; isArrow _ = False

fromArrow :: Type -> Maybe Type
fromArrow (Arrow t) = t

arity :: Type -> Int
arity (Type _ ts) = length ts
arity (Func is _ _) = length is
arity (Var _) = 0
arity (Cons _ ts) = length ts
//TODO arity of Uniq / Forall / Arrow?

constructorsToFunctions :: TypeDef -> [(String,Type,Maybe Priority)]
constructorsToFunctions {td_name,td_uniq,td_args,td_rhs=TDRCons _ conses}
	= [(c.cons_name, Func c.cons_args return c.cons_context, c.cons_priority) \\ c <- conses]
where return = if td_uniq Uniq id $ Type td_name td_args
constructorsToFunctions {td_name,td_uniq,td_args,td_rhs=TDRMoreConses conses}
	= [(c.cons_name, Func c.cons_args return c.cons_context, c.cons_priority) \\ c <- conses]
where return = if td_uniq Uniq id $ Type td_name td_args
constructorsToFunctions _ = []

recordsToFunctions :: TypeDef -> [(String,Type)]
recordsToFunctions {td_name,td_uniq,td_args,td_rhs=TDRRecord _ _ fields}
	= [(f.rf_name, Func [arg] f.rf_type []) \\ f <- fields]
where arg = if td_uniq Uniq id $ Type td_name td_args
recordsToFunctions _ = []

td_name :: TypeDef -> String
td_name {td_name} = td_name

typedef :: String Bool [Type] TypeDefRhs -> TypeDef
typedef name uniq args rhs
	= {td_name=name, td_uniq=uniq, td_args=args, td_rhs=rhs}

constructor :: String [Type] [TypeVar] TypeContext (Maybe Priority) -> Constructor
constructor name args exi_vars tc pri
	= {cons_name=name, cons_args=args, cons_exi_vars=exi_vars, cons_context=tc, cons_priority=pri}

recordfield :: String Type -> RecordField
recordfield selector type = {rf_name=selector, rf_type=type}

removeDupTypedefs :: [TypeDef] -> [TypeDef]
removeDupTypedefs [] = []
removeDupTypedefs [td:tds]
	= [td:removeDupTypedefs $ filter (\d -> d.td_name <> td.td_name) tds]
