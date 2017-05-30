implementation module TypeUtil

import TypeDef

import StdArray, StdBool, StdOrdList, StdString, StdTuple
from StdFunc import flip, o

import Control.Applicative
import Control.Monad
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
from Text import class Text (concat), instance Text String
from GenEq import generic gEq, ===

(--) infixr 1 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b
(+-) infixr 1 :: a b -> [String] | print a & print b
(+-) a b = print True a ++ print False b
(-+) infixr 1 :: a b -> [String] | print a & print b
(-+) a b = print False a ++ print True b
(+-+) infixr 1 :: a b -> [String] | print a & print b
(+-+) a b = print True a ++ print True b

printersperse :: Bool a [b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

instance toInt Bool where toInt True = 1; toInt False = 0

instance print String where print _ s = [s]
instance print Int where print _ i = [toString i]
instance print Char where print _ c = [{c}]
instance print [a] | print a
where print _ xs = [concat e \\ e <- map (print False) xs]

instance print (Maybe a) | print a
where print _ Nothing = []; print b (Just x) = print b x

instance print Kind
where
	print _ KindConst = ["*"]
	print b (KindArrow ks) = parlft -- printersperse True "->" (ks ++ [KindConst]) -- parrgt
	where (parlft,parrgt) = if b ("(",")") ("","")

instance print ClassOrGeneric
where
	print _ (Class s) = [s]
	print _ (Generic n k) = n -- "{|" -- k -- "|}"

instance print ClassRestriction where print _ (cog, v) = cog -- " " -- v

instance print ClassContext
where
	print _ [] = []
	print _ crs = printersperse False " & "
		[printersperse False ", " (map fst gr) -- " " -- snd (hd gr) \\ gr <- grps]
	where
		grps = groupBy (\a b -> snd a == snd b) crs

instance print Type
where
	print isArg (Type s vs)
		// Lists
		| s == "_List"   = "[" -- vs -- "]"
		| s == "_!List"  = if (isEmpty vs) ["[! ]"] ("[!" -- vs -- "]")
		| s == "_List!"  = if (isEmpty vs) ["[ !]"] ("[" -- vs -- "!]")
		| s == "_!List!" = "[!" -- vs -- "!]"
		| s == "_#List"  = "[#" -- vs -- "]"
		| s == "_#List!" = "[#" -- vs -- "!]"
		// Arrays
		| s == "_#Array" = "{#" -- vs -- "}"
		| s == "_Array"  = "{" -- vs -- "}"
		| s == "_!Array" = "{!" -- vs -- "}"
		// Tuples
		| s % (0,5) == "_Tuple"
			# n = toInt (s % (6, size s - 1))
			| isEmpty vs = "(" -- repeatn (n-1) ',' -- ")"
			| n > length vs = "((" -- repeatn (n-1) ',' -- ") " -- printersperse True " " vs -- ")"
			| otherwise     = "(" -- printersperse False ", " vs -- ")"
		// Other predefined types
		| s == "_Unit"   = ["()"]
		| s.[0] == '_'   = [s % (1, size s - 1)]
		// Other types
		| isEmpty vs     = print isArg s
		| otherwise      = parens isArg (s -- " " -- printersperse True " " vs)
	print _ (Var v) = [v]
	print ia (Func [] r []) = print ia r
	print _ (Func [] r cc) = r -- " " -- cc
	print ia (Func ts r []) = parens ia (printersperse True " " ts -- " -> " -- r)
	print _ (Func ts r cc) = (Func ts r []) -- " | " -- cc
	print ia (Cons tv [])  = print ia tv
	print ia (Cons tv ats) = parens ia (tv -- " " -- printersperse True " " ats)
	print _ (Uniq t)       = "*" -+ t
	print _ (Forall tvs t []) = "(A." -- printersperse True " " tvs -- ": " -- t -- ")"
	print _ (Forall tvs t cc) = "(A." -- printersperse True " " tvs -- ": " -- t -- " " -- cc -- ")"
	print _ (Arrow Nothing)  = ["(->)"]
	print _ (Arrow (Just t)) = "((->) " -+ t +- ")"

parens :: Bool [String] -> [String]
parens False ss = ss
parens True ss  = ["(":ss] ++ [")"]

instance print TypeDef
where
	print _ {td_name,td_uniq,td_args,td_rhs}
		= ":: " -- if td_uniq "*" "" -- td_name -- " " --
			printersperse True " " td_args -- if (isEmpty td_args) "" " " --
			case td_rhs of
				(TDRCons ext cs) = "= " -- makeADT ext cs
				(TDRRecord _ exi fields) = "= " --
					if (isEmpty exi) [] ("E." -- printersperse False " " exi -- ": ") --
					makeRecord exi fields
				(TDRSynonym t) = ":== " -- t
				TDRAbstract = []
				(TDRAbstractSynonym t) = "(:== " -- t -- ")"
	where
		indent = size td_name + toInt td_uniq + length td_args + sum (map (size o concat o print True) td_args)
		recordIndent exi = repeatn (indent + 6 + if (isEmpty exi) 0 (3 + length exi + sum (map size exi))) ' '
		consIndent = repeatn (indent + 4) ' '

		makeRecord :: [TypeVar] [RecordField] -> String
		makeRecord _ [] = "{}"
		makeRecord exi [f1:fs]
			= concat ("{ " -- printRf f1 -- "\n" --
				concat [concat (recordIndent exi -- ", " -- printRf f -- "\n")
				        \\ f <- fs] -- recordIndent exi -- "}")
		where
			padLen = maxList (map (\f -> size f.rf_name) [f1:fs])
			pad i s = s +++ toString (repeatn (i - size s) ' ')

			printRf {rf_name,rf_type} = pad padLen rf_name -- " :: " -- rf_type

		makeADT :: Bool [Constructor] -> String
		makeADT exten [] = if exten " .." ""
		makeADT False [c1:cs]
			= concat (c1 -- "\n" --
				concat [concat (consIndent -- "| " -- c -- "\n") \\ c <- cs])
		makeADT True cs = concat (makeADT False cs -- consIndent -- "| ..")

instance print Constructor
where
	print _ {cons_name,cons_args,cons_exi_vars=evars,cons_context,cons_priority}
		= if (isEmpty evars) [] ("E." -- printersperse False " " evars -- ": ") --
			cons_name -- " " -- prio -- printersperse True " " cons_args --
			if (isEmpty cons_context) [] (" & " -- cons_context)
	where
		prio = case cons_priority of
			Nothing -> []
			Just p  -> p -- " "

instance print Priority
where
	print _ (LeftAssoc i)  = "infixl " -- i
	print _ (RightAssoc i) = "infixr " -- i
	print _ (NoAssoc i)    = "infix " -- i

propagate_uniqueness :: Type -> Type
propagate_uniqueness (Type t ts)
	# ts = map propagate_uniqueness ts
	= if (any isUniq ts) (Uniq (Type t ts)) (Type t ts)
propagate_uniqueness (Func is r cc)
	= Func (map propagate_uniqueness is) (propagate_uniqueness r) cc
propagate_uniqueness (Cons v ts)
	# ts = map propagate_uniqueness ts
	= if (any isUniq ts) (Uniq (Cons v ts)) (Cons v ts)
propagate_uniqueness (Forall vs t cc)
	= Forall vs (propagate_uniqueness t) cc
propagate_uniqueness t
	= t

import StdMisc

resolve_synonyms :: [TypeDef] Type -> ([TypeDef], Type)
resolve_synonyms tds (Type t ts)
	# (syns, ts) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) ts
	= case candidates of
		[]
			= (syns, Type t ts)
		[syn=:{td_args, td_rhs=TDRSynonym synt}:_]
			# newargs = map ((+++) "__" o fromVar) td_args
			# (Just t)
				= assignAll [(fromVar a, Var n) \\ a <- td_args & n <- newargs] synt
				>>= assignAll [(a,r) \\ a <- newargs & r <- ts]
			| length td_args <> length ts
				# (Type r rs) = t
				# t = Type r $ rs ++ drop (length td_args) ts
				= appFst ((++) [syn:syns]) $ resolve_synonyms tds t
			= ([syn:syns], t)
where
	candidates = [td \\ td=:{td_rhs=TDRSynonym syn} <- tds
		| td.td_name == t && length td.td_args <= length ts
		&& (isType syn || length td.td_args == length ts)]
resolve_synonyms tds (Func is r cc)
	# (syns, [r:is]) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) [r:is]
	= (syns, Func is r cc)
resolve_synonyms tds (Cons v ts)
	# (syns, ts) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) ts
	= (syns, Cons v ts)
resolve_synonyms tds (Forall vs t cc)
	# (syns, t) = resolve_synonyms tds t
	= (syns, Forall vs t cc)
resolve_synonyms tds (Arrow (Just t))
	= appSnd (Arrow o pure) $ resolve_synonyms tds t
resolve_synonyms tds t
	= ([], t)

// Apply a TVAssignment to a Type
assign :: !TVAssignment !Type -> Maybe Type
assign va (Type s ts) = Type s <$^> map (assign va) ts
assign va (Func ts r cc) = Func <$^> map (assign va) ts
		>>= (\f->f <$> assign va r) >>= (\f->pure $ f cc) // TODO cc
assign (v,a) (Var v`) = pure $ if (v == v`) a (Var v`)
assign va=:(v,Type s ts) (Cons v` ts`)
	| v == v`   = Type s <$^> map (assign va) (ts ++ ts`)
	| otherwise = Cons v` <$^> map (assign va) ts`
assign va=:(v,Cons c ts) (Cons v` ts`)
	| v == v`   = Cons c <$^> map (assign va) (ts ++ ts`)
	| otherwise = Cons v` <$^> map (assign va) ts`
assign va=:(v,Var v`) (Cons v`` ts)
	| v == v``  = Cons v` <$^> map (assign va) ts
	| otherwise = Cons v`` <$^> map (assign va) ts
assign va=:(v,_) (Cons v` ts)
	| v == v` = empty
	| otherwise = Cons v` <$^> map (assign va) ts
assign va (Uniq t) = Uniq <$> (assign va t)
assign va=:(v,Var v`) (Forall tvs t cc)
	= Forall <$^> map (assign va) tvs >>= (\f -> flip f cc <$> assign va t)
assign va=:(v,_) (Forall tvs t cc)
	| isMember (Var v) tvs = empty
	| otherwise = flip (Forall tvs) cc <$> assign va t

(<$^>) infixl 4 //:: ([a] -> b) [Maybe a] -> Maybe b
(<$^>) f mbs :== ifM (all isJust mbs) $ f $ map fromJust mbs

//ifM :: Bool a -> m a | Alternative m
ifM b x :== if b (pure x) empty
