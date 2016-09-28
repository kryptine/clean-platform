/// Blazingly fast parsers on `String`s.
///
/// This library provides an aplicative interface to parser combinators
/// specialized to the `String` type. It is loosely modeld after the Attoparsec
/// library in Haskell. Names are chosen te read easily. Most of the time they
/// resable the names of Attoparsec, but not always!
///
/// - Author: Tim Steenvoorden
definition module Text.Parsers.String

from Data.Char import isAlpha, isLetter, isUpper, isLower

from Data.Either import :: Either
from Data.Maybe import :: Maybe

import Data.Functor
import Control.Applicative
// from Data.Functor import class Functor
// from Control.Applicative import class Applicative, class Alternative

from Data.String.Slice import :: Slice

from StdOverloaded import class ~

////////////////////////////////////////////////////////////////////////////////
/// # Types
////////////////////////////////////////////////////////////////////////////////

:: Parser a =: Parser (Slice -> Result a) //FIXME should be abstract, but not possible with newtypes?
:: Result a
:: Message :== String

////////////////////////////////////////////////////////////////////////////////
/// # Running
////////////////////////////////////////////////////////////////////////////////

/// Run a `Parser` on a `String`.
parseOnly ::
    /// Parser to be run
    (Parser a)
    /// String to parse
    String
    /// Error message or parse result
    -> Either Message a

// maybeResult :: (ParseResult a) -> Maybe a
// eitherResult :: (ParseResult a) -> Either Message a

////////////////////////////////////////////////////////////////////////////////
/// # Instances
////////////////////////////////////////////////////////////////////////////////

instance Functor Parser
instance Applicative Parser
instance Alternative Parser

////////////////////////////////////////////////////////////////////////////////
/// # Individual characters
////////////////////////////////////////////////////////////////////////////////

/// Parse a character that satisfies a given condition.
satisfy ::
    /// Condition on character
    (Char -> Bool)
    /// Parser of matched character
    -> Parser Char

/// Parse a matching character.
char :: Char -> Parser Char

/// Parse any character except matching.
notChar :: Char -> Parser Char

/// Parse any character.
///
/// Advances the parser one character.
anyChar :: Parser Char

/// Parse a digit.
digit :: Parser Char

////////////////////////////////////////////////////////////////////////////////
/// # Special characters
////////////////////////////////////////////////////////////////////////////////

isSpace :: Char -> Bool
isHorizontalSpace :: Char -> Bool
isVerticalSpace :: Char -> Bool
isAnySpace :: Char -> Bool
isEndOfLine :: Char -> Bool

space :: Parser Char
horizontalSpace :: Parser Char
verticalSpace :: Parser Char
anySpace :: Parser Char

blank :: Parser ()
endOfLine :: Parser ()
restOfLine :: Parser ()
endOfInput :: Parser ()

////////////////////////////////////////////////////////////////////////////////
/// # String parsing
////////////////////////////////////////////////////////////////////////////////

/// Parse a matching string.
string :: String -> Parser String

/// Parse a word.
///
/// Characters that form a word are defined by `isLetter` in `Data.Char`.
///
/// - seealso: `Data.Char.isLetter`
/// - note: Not in Attoparsec
word :: Parser String

/// Parse a string written between two characters.
///
/// - note: Not in Attoparsec
between :: Char Char -> Parser String

/// ## Given number

take :: Int -> Parser String
skip :: Int -> Parser ()

/// ## While predicate holds

takeWhile :: (Char -> Bool) -> Parser String
takeWhile1 :: (Char -> Bool) -> Parser String
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile1 :: (Char -> Bool) -> Parser ()

/// ## Till predicate holds

takeTill :: (Char -> Bool) -> Parser String
takeTill1 :: (Char -> Bool) -> Parser String
skipTill :: (Char -> Bool) -> Parser ()
skipTill1 :: (Char -> Bool) -> Parser ()

/// ## Spaces

skipSpace :: Parser ()
skipHorizontalSpace :: Parser ()
skipVerticalSpace :: Parser ()
skipAnySpace :: Parser ()

skipSpace1 :: Parser ()
skipHorizontalSpace1 :: Parser ()
skipVerticalSpace1 :: Parser ()

////////////////////////////////////////////////////////////////////////////////
/// # Numeric parsing
////////////////////////////////////////////////////////////////////////////////

decimal :: Parser Int
hexadecimal :: Parser Int
octal :: Parser Int
scientific :: Parser (Int, Int)
signed :: (Parser a) -> Parser a | ~ a

nat :: Parser Int //FIXME should be Nat
int :: Parser Int
real :: Parser Real

////////////////////////////////////////////////////////////////////////////////
/// # Combinators
////////////////////////////////////////////////////////////////////////////////

choice :: [f a] -> f a | Alternative  f
count :: !Int (f a) -> f [a] | Alternative f
option :: a (f a) -> f a | Alternative f
// many :: (f a) -> f [a] | Alternative f
many1 :: (f a) -> f [a] | Alternative f
manyTill :: (f a) (f b) -> f [a] | Alternative f
// sepBy :: (f a) (f s) -> f [a] | Alternative f
sepBy1 :: (f a) (f s) -> f [a] | Alternative f
/// Like `many`, but skip zero or more instances of an action.
skipMany :: (f a) -> f () | Alternative f
/// Like `many1`, but skip one or more instances of an action.
skipMany1 :: (f a) -> f () | Alternative f
