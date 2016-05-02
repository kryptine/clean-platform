definition module Text.Parsers.Simple.ParserCombinators

from Control.Applicative import class Applicative (..), class Alternative (..), *>, <*
from Control.Monad import class Monad (..)
from Data.Either import :: Either (..)
from Data.Functor import class Functor (..), <$>

:: Error :== String
:: Parser t a

// AMF instances
instance Functor (Parser t)
instance Applicative (Parser t)
instance Alternative (Parser t)
instance Monad (Parser t)

// Functions to run the parser
parse     :: (Parser t a) [t] -> Either [Error] a
runParser :: (Parser t a) [t] -> ([(a, [t])], [Error])

// Core combinators
pFail    :: Parser t a
pYield   :: a -> Parser t a
pSatisfy :: (t -> Bool) -> Parser t t
pError   :: Error -> Parser t a

// Convenience parsers
(@!) infixr 4 :: (Parser t a) Error -> Parser t a
(<$) infixl 6 :: a (Parser t b) -> Parser t a
($>) infixl 6 :: (Parser t b) a -> Parser t a

(<<|>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a
(<|>>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a

(<:>) infixr 6 :: (Parser s r) (Parser s [r]) -> Parser s [r]

pMany     :: (Parser s r) -> Parser s [r]
pSome     :: (Parser s r) -> Parser s [r]
pOptional :: (Parser s r) (r -> Parser s r) -> Parser s r
pOneOf    :: [t] -> Parser t t | == t
pChainl   :: (Parser t a) (Parser t (a a -> a)) a -> Parser t a
pChainl1  :: (Parser t a) (Parser t (a a -> a)) -> Parser t a
pToken    :: t -> Parser t t | == t
pSepBy    :: (Parser t a) (Parser t s) -> Parser t [a]
pSepBy1   :: (Parser t a) (Parser t s) -> Parser t [a]
pList     :: [Parser t a] -> Parser t [a]

pUpper    :: Parser Char Char
pLower    :: Parser Char Char
pAlpha    :: Parser Char Char
pAlphanum :: Parser Char Char
pDigit    :: Parser Char Char
pOctDigit :: Parser Char Char
pHexDigit :: Parser Char Char
pSpace    :: Parser Char Char
pControl  :: Parser Char Char
pPrint    :: Parser Char Char
pAscii    :: Parser Char Char

pPOpen      :: Parser Char Char
pPClose     :: Parser Char Char
pBOpen      :: Parser Char Char
pBClose     :: Parser Char Char
pCOpen      :: Parser Char Char
pCClose     :: Parser Char Char
pComma      :: Parser Char Char
pSemiCol    :: Parser Char Char
pLt         :: Parser Char Char
pGt         :: Parser Char Char
pEq         :: Parser Char Char
pAmp        :: Parser Char Char
pPipe       :: Parser Char Char
pDash       :: Parser Char Char
pUnderscore :: Parser Char Char
