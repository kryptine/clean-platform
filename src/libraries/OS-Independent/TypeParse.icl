implementation module TypeParse

from StdFunc import o
import StdList
import StdString
import StdTuple

import Data.Either
import Data.Maybe
from Data.Func import $
from Data.List import instance Functor []
from Text import class Text(concat), instance Text String
import Data.Functor
import Control.Applicative
import Control.Monad

import GenEq

import TypeDef
import TypeUtil
import Yard

(|<<) infixl 1 :: (m a) (m b) -> m a | Monad m
(|<<) ma mb = ma >>= \a -> mb >>= \_ -> pure a

derive gEq Token
instance == Token where == a b = a === b

:: Token
	= TIdent String            // UpperCaseId or FunnyId
	| TVar String              // LowerCaseId

	| TArrow                   // ->
	| TComma                   // ,
	| TStar                    // *
	| TAnonymous               // .
	| TUnboxed                 // #
	| TStrict                  // !
	| TColon                   // :
	| TUniversalQuantifier     // A.
	| TPipe                    // |
	| TAmpersand               // &

	| TParenOpen | TParenClose // ( )
	| TBrackOpen | TBrackClose // [ ]
	| TBraceOpen | TBraceClose // { }

isTIdent (TIdent _) = True; isTIdent _ = False
isTVar   (TVar   _) = True; isTVar   _ = False

tokenize :: ([Char] -> Maybe [Token])
tokenize = fmap reverse o tkz []
where
	tkz :: [Token] [Char] -> Maybe [Token]
	tkz tks [] = Just tks
	tkz tks ['-':'>':cs] = tkz [TArrow:tks]               cs
	tkz tks [',':cs]     = tkz [TComma:tks]               cs
	tkz tks ['*':cs]     = tkz [TStar:tks]                cs
	tkz tks ['.':cs]     = tkz [TAnonymous:tks]           cs
	tkz tks ['#':cs]     = tkz [TUnboxed:tks]             cs
	tkz tks ['!':cs]     = tkz [TStrict:tks]              cs
	tkz tks ['(':cs]     = tkz [TParenOpen:tks]           cs
	tkz tks [')':cs]     = tkz [TParenClose:tks]          cs
	tkz tks ['[':cs]     = tkz [TBrackOpen:tks]           cs
	tkz tks [']':cs]     = tkz [TBrackClose:tks]          cs
	tkz tks ['{':cs]     = tkz [TBraceOpen:tks]           cs
	tkz tks ['}':cs]     = tkz [TBraceClose:tks]          cs
	tkz tks ['A':'.':cs] = tkz [TUniversalQuantifier:tks] cs
	tkz tks [':':cs]     = tkz [TColon:tks]               cs
	tkz tks ['|':cs]     = tkz [TPipe:tks]                cs
	tkz tks ['&':cs]     = tkz [TAmpersand:tks]           cs
	tkz tks [c:cs]
	| isSpace c = tkz tks cs
	| isUpper c = tkz [TIdent $ toString [c:id]:tks] cs`
		with (id, cs`) = span isIdentChar cs
	| isFunny c = tkz [TIdent $ toString [c:id]:tks] cs`
		with (id, cs`) = span isFunny cs
	| isLower c = tkz [TVar $ toString [c:var]:tks] cs`
		with (var, cs`) = span isIdentChar cs
	tkz _ _ = Nothing

	isIdentChar :: Char -> Bool
	isIdentChar c = any (\f->f c) [isLower, isUpper, isDigit, (==)'_', (==) '`']

	isFunny :: Char -> Bool
	isFunny c = isMember c ['~@#$%^?!+-*<>\\/|&=:']

type :: Parser Token Type
type = liftM3 Func (some argtype) (item TArrow >>| type) optContext
	<|> liftM2 Cons cons (some argtype)
	<|> liftM2 Type ident (many argtype)
	<|> liftM3 Forall
		(item TUniversalQuantifier >>| some argtype |<< item TColon)
		type
		optContext
	<|> argtype
where
	argtype :: Parser Token Type
	argtype = (item TParenOpen >>| item TParenClose >>| pure (Type "_Unit" []))
		<|> parenthised type
		<|> liftM (\t -> Type t []) ident
		<|> liftM Var var
		<|> liftM Uniq uniq
		<|> liftM (\t -> Type "_#Array" [t]) (braced  (item TUnboxed >>| type))
		<|> liftM (\t -> Type "_!Array" [t]) (braced  (item TStrict  >>| type))
		<|> liftM (\t -> Type "_Array"  [t]) (braced  type)
		<|> liftM (\t -> Type "_#List!" [t]) (bracked (item TUnboxed >>| type |<< item TStrict))
		<|> liftM (\t -> Type "_!List!" [t]) (bracked (item TStrict  >>| type |<< item TStrict))
		<|> liftM (\t -> Type "_#List"  [t]) (bracked (item TUnboxed >>| type))
		<|> liftM (\t -> Type "_!List"  [t]) (bracked (item TStrict  >>| type))
		<|> liftM (\t -> Type "_List!"  [t]) (bracked (type |<< item TStrict))
		<|> liftM (\t -> Type "_List"   [t]) (bracked type)
		<|> liftM (\ts -> Type ("_Tuple" +++ toString (length ts)) ts)
			(parenthised (seplist TComma type))
		<|> (item TStrict >>| argtype)       // ! ignored for now
		<|> (item TAnonymous >>| argtype)    // . ignored for now
		<|> (unqvar >>| item TColon >>| argtype) // u: & friends ignored for now

	ident :: Parser Token String
	ident = (\(TIdent id)->id) <$> satisfy isTIdent

	var :: Parser Token TypeVar
	var = (\(TVar var)->var) <$> satisfy isTVar
	cons = var
	unqvar = var

	uniq :: Parser Token Type
	uniq = item TStar >>| argtype

	seplist :: a (Parser a b) -> Parser a [b] | Eq a
	seplist sep p = liftM2 (\es e-> es ++ [e]) (some (p |<< item sep)) p
		<|> liftM pure p
		<|> pure empty

	optContext :: Parser Token TypeContext
	optContext = context <|> pure []

	context :: Parser Token TypeContext
	context = item TPipe >>| flatten <$> seplist TAmpersand context`
	where
		context` :: Parser Token TypeContext
		context` = seplist TComma classOrGeneric >>= \restrictions ->
			some argtype >>= \ts ->
			mapM (flip ($) ts) restrictions

		classOrGeneric :: Parser Token ([Type] -> Parser Token TypeRestriction)
		classOrGeneric = className >>= \name ->
			optional (braced $ piped skipKind) >>= \kind ->
			case kind of
				Nothing -> pure $ pure o Instance name
				Just _  -> pure $ deriv name
		where
			deriv :: String [Type] -> Parser Token TypeRestriction
			deriv d [t] = pure $ Derivation d t
			deriv _ _   = empty

		className :: Parser Token String
		className = ident <|> var

		skipKind :: Parser Token [Token]
		skipKind = some $ satisfy \t -> case t of
			TStar       -> True
			TArrow      -> True
			TParenOpen  -> True
			TParenClose -> True
			_           -> False

	parenthised p = item TParenOpen >>| p |<< item TParenClose
	braced p = item TBraceOpen >>| p |<< item TBraceClose
	bracked p = item TBrackOpen >>| p |<< item TBrackClose
	piped p = item TPipe >>| p |<< item TPipe

parseType :: [Char] -> Maybe Type
parseType cs
# mbTokens = tokenize cs
| isNothing mbTokens = Nothing
= case runParser type (fromJust mbTokens) of
	(Right t, []) -> Just t
	_             -> Nothing
