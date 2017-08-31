implementation module TypeUnify

import TypeDef, TypeUtil

import StdOrdList

from StdFunc import o, flip
from StdMisc import abort
import StdBool
import StdList
import StdString
import StdTuple
import StdArray
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

derive gEq Type, TypeRestriction, Kind

prepare_unification :: !Bool /* is left */ [TypeDef] !Type -> ([TypeDef], Type)
prepare_unification b db (Func [] t _) = prepare_unification b db t
prepare_unification isleft db t
# (syns, t) = resolve_synonyms db t
# t = propagate_uniqueness t
# t = reduceArities t
# t = appendToVars (if isleft "l" "r") t
= (syns, t)
where
	appendToVars :: String Type -> Type
	appendToVars s t = fromJust $ assignAll (map rename $ allVars t) t
	where rename v = (v, Var (v+++s))

finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier
finish_unification syns tvs
# (tvs1, tvs2) = (filter (endsWith "l") tvs, filter (endsWith "r") tvs)
# (tvs1, tvs2) = (map removeEnds tvs1, map removeEnds tvs2)
= {left_to_right=tvs1, right_to_left=tvs2, used_synonyms=removeDupTypedefs syns}
where
	endsWith :: String TVAssignment -> Bool
	endsWith n (h,_) = h % (size h - size n, size h - 1) == n

	removeEnds :: TVAssignment -> TVAssignment
	removeEnds (v,t) = let rm s = s % (0, size s - 2) in (rm v, fromJust $
	                   assignAll (map (\v->(v,Var (rm v))) $ allVars t) t)

unify :: !Type !Type -> Maybe [TVAssignment]
unify t1 t2
	= unify2 $ toMESystem t1 t2

:: MultiEq = ME ![TypeVar] ![Type]

:: MESystem = { solved   :: ![MultiEq]
              , unsolved :: ![(Int, MultiEq)]
              }

allAssignments :: MultiEq -> [TVAssignment]
allAssignments (ME vs ts) = [(v,t) \\ v <- vs, t <- ts]

toMESystem :: !Type !Type -> MESystem
toMESystem t1 t2
	= { solved   = []
	  , unsolved = [(0, ME ["type"] [t1,t2])]
	  }

instance == MultiEq where (==) (ME a b) (ME c d) = a == c && b == d

unify2 :: !MESystem -> Maybe [TVAssignment]
unify2 {solved,unsolved}
# unsolved = sortBy (\(a,b) (c,d) -> a < c) unsolved
| isEmpty unsolved     = Just $ solution solved
# (count, me=:(ME vars types)) = hd unsolved
| count <> 0           = Nothing // cycle
# unsolved             = tl unsolved
| isEmpty types
	= unify2 {solved=[me:solved],unsolved=removeFromCounters vars unsolved}
# cPaF                 = commonPartAndFrontier types
| isNothing cPaF       = Nothing // clash
# (cPart,frontier)     = fromJust cPaF
// MultiEq reduction
# unsolved             = updateCounters $ compactify $
                         unsolved ++ [(0,f) \\ f <- frontier]
# solved               = [ME vars [cPart]:solved]
// Check universal quantifiers
# univars              = flatten $ map allUniversalVars types
| any
	(\(v,t) -> not (isVar t) && isMember v univars)
	(flatten (map (allAssignments o snd) unsolved))
                       = Nothing // Universally quantified var was assigned
= unify2 {solved=solved,unsolved=unsolved}
where
	solution :: [MultiEq] -> [TVAssignment]
	solution [] = []
	solution [ME [v:vs] ts:mes]
		= [(v,t) \\ t <- ts] ++ [(v`,Var v) \\ v` <- vs] ++ solution mes

	removeFromCounters :: ![TypeVar] ![(Int,MultiEq)] -> [(Int,MultiEq)]
	removeFromCounters vs [] = []
	removeFromCounters vs [(i,me=:(ME _ ts)):mes]
		= [(i - sum (map (count vs) ts),me):removeFromCounters vs mes]
	where
		count :: ![TypeVar] !Type -> Int
		count vs (Var v) = if (isMember v vs) 1 0
		count vs (Cons v ts) = if (isMember v vs) 1 0 + sum (map (count vs) ts)
		count vs (Type _ ts) = sum $ map (count vs) ts
		count vs (Func is r _) = sum $ map (count vs) [r:is]
		count vs (Uniq t) = count vs t

	updateCounters :: [(Int, MultiEq)] -> [(Int, MultiEq)]
	updateCounters eqs
		= [(sum (map (count mes) vars), me) \\ me=:(ME vars _) <- mes]
	where
		mes = map snd eqs

		count :: [MultiEq] TypeVar -> Int
		count mes v
			= sum [length (filter (isVarOrCons` v) (flatten (map subtypes ts)))
			       \\ (ME _ ts) <- mes]

	compactify :: [(Int, MultiEq)] -> [(Int, MultiEq)]
	compactify [] = []
	compactify [(c,ME vars types):mes] = case lookup vars mes of
		Nothing = [(c,ME vars types):compactify mes]
		(Just me=:(c`,ME vars` types`))
			# vars = removeDup $ vars ++ vars`
			# types = removeDup $ types ++ types`
			= compactify [(c+c`, ME vars types):removeMember me mes]
	where
		lookup :: [TypeVar] [(Int,MultiEq)] -> Maybe (Int, MultiEq)
		lookup vs [] = Nothing
		lookup vs [me=:(i,ME vars _):mes]
		| isEmpty (intersect vs vars) = lookup vs mes
		= Just me

:: CommonPart :== Type
:: Frontier :== [MultiEq]

commonPartAndFrontier :: [Type] -> Maybe (CommonPart, Frontier)
commonPartAndFrontier ts
| isEmpty ts = Nothing
| any isForall ts // TODO class context
	= commonPartAndFrontier $ map (\t -> if (isForall t) (fromForall t) t) ts
| any isVar ts = Just (hd $ filter isVar ts, makemulteq ts)
| all isType ts
	# names = map (\(Type n _) -> n) ts
	| (<>) 1 $ length $ removeDup $ names = Nothing
	# name = hd names
	# lengths = map (length o (\(Type _ ts) -> ts)) ts
	| (<>) 1 $ length $ removeDup $ lengths = Nothing
	# len = hd lengths
	| len == 0 = Just (Type name [], [])
	# args = map (\(Type _ ts) -> ts) ts
	# cpafs = [commonPartAndFrontier [a!!i \\ a <- args] \\ i <- [0..len-1]]
	| any isNothing cpafs = Nothing
	# (cps, fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	= Just (Type name cps, flatten fronts)
| all isFunc ts // TODO class context
	# types = map (\(Func is t _) -> [t:is]) ts
	# lengths = map length types
	| (<>) 1 $ length $ removeDup $ lengths = Nothing
	# len = hd lengths
	# cpafs = [commonPartAndFrontier [t!!i \\ t <- types] \\ i <- [0..len-1]]
	| any isNothing cpafs = Nothing
	# ([cp:cps], fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	= Just (Func cps cp [], flatten fronts)
| all isCons ts
	# lengths = [length ts \\ (Cons _ ts) <- ts]
	| 1 == length (removeDup lengths)
		// All same arity, pairwise unification
		# len = hd lengths
		# lists = map (\(Cons v ts) -> [Var v:ts]) ts
		# cpafs = [commonPartAndFrontier [l!!i \\ l <- lists] \\ i <- [0..len]]
		| any isNothing cpafs = Nothing
		# ([Var cp:cps], fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
		= Just (Cons cp cps, flatten fronts)
	// Different arities, curry in some arguments
	# (minlen,maxlen) = (minList lengths, maxList lengths)
	# maxvar = hd [v \\ (Cons v ts) <- ts | length ts == maxlen]
	# splits = [splitAt (length ts - minlen) ts \\ (Cons v ts) <- ts]
	# types = [if (isEmpty init) (Var v) (Cons v init) \\ (init,_) <- splits & (Cons v _) <- ts]
	# rests = map snd splits
	# cpafs = [commonPartAndFrontier [r!!i \\ r <- rests] \\ i <- [0..minlen - 1]]
	| any isNothing cpafs = Nothing
	# (cps, fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	# cpaf = commonPartAndFrontier types
	| isNothing cpaf = Nothing
	# (cp, front) = fromJust cpaf
	| isCons cp
		# (Cons cpv cpts) = cp
		= Just (Cons cpv (cpts ++ cps), flatten [front:fronts])
	# (Var v) = cp
	= Just (Cons v cps, flatten [front:fronts])
| all (\t -> isCons t || isType t) ts
	# types = filter isType ts
	# conses = filter isCons ts
	// Unify types separately
	# cpaft = commonPartAndFrontier types
	| isNothing cpaft = Nothing
	# (cpt=:(Type cptn cptts), frontt) = fromJust cpaft
	// Unify conses separately
	# cpafc = commonPartAndFrontier conses
	| isNothing cpafc = Nothing
	# (cpc, frontc) = fromJust cpafc
	// Merge results
	| isVar cpc = let (Var cpc`) = cpc in
		Just (cpt, [ME [cpc`] [cpt]] ++ frontt ++ frontc)
	# (Cons cpcv cpcts) = cpc
	| length cpcts > length cptts = Nothing
	# (cptts_curry, cptts_unify) = splitAt (length cptts - length cpcts) cptts
	# cpafs = [commonPartAndFrontier [t,c] \\ t <- cptts_unify & c <- cpcts]
	| any isNothing cpafs = Nothing
	# (cps,fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	# cps = cptts_curry ++ cps
	| isEmpty cps = Just (Var cpcv, flatten fronts ++ frontt ++ frontc)
	= Just (Cons cpcv cps, flatten fronts ++ [ME [cpcv] [Type cptn cptts_curry]] ++ frontt ++ frontc)
| all isUniq ts
	= (\(cpaf,front) -> (Uniq cpaf,front))
		<$> commonPartAndFrontier (map (\(Uniq t) -> t) ts)
| otherwise = Nothing
where
	makemulteq :: [Type] -> Frontier
	makemulteq ts = let (vs,ts`) = partition isVar ts in [ME (map fromVar vs) ts`]

//-----------------------//
// Unification utilities //
//-----------------------//

// Make all functions arity 1 by transforming a b -> c to a -> b -> c
reduceArities :: !Type -> Type
reduceArities (Func ts r tc)
	| length ts > 1 = Func [reduceArities $ hd ts] (reduceArities $ Func (tl ts) r tc) tc
	| otherwise = Func (map reduceArities ts) (reduceArities r) tc
reduceArities (Type s ts) = Type s $ map reduceArities ts
reduceArities (Cons v ts) = Cons v $ map reduceArities ts
reduceArities (Uniq t) = Uniq $ reduceArities t
reduceArities (Var v) = Var v
reduceArities (Forall tvs t tc) = Forall tvs (reduceArities t) tc
