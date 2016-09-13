implementation module Text.Parsers.String

import StdEnv

import Data.Either
import Data.Maybe
import Data.Func

import Data.Functor
import Control.Applicative

from Data.Foldable import instance Foldable []
import Data.Traversable

import qualified Data.String as String
import qualified Data.String.Slice as Slice
from Data.String.Slice import :: Slice
import qualified Data.List as List
from Data.List import instance Functor []

////////////////////////////////////////////////////////////////////////////////
/// # Types
////////////////////////////////////////////////////////////////////////////////

:: Parser a =: Parser (Slice -> Result a)
:: Result a
    = Done Slice a
    | Fail Slice Message
:: Message :== String

////////////////////////////////////////////////////////////////////////////////
/// # Running
////////////////////////////////////////////////////////////////////////////////

parseOnly :: (Parser a) String -> Either Message a
parseOnly p s = eitherResult $ parse p $ 'Slice'.wrap s

maybeResult :: !(Result a) -> Maybe a
maybeResult (Done _ a) = Just a
maybeResult (Fail _ _) = Nothing

eitherResult :: !(Result a) -> Either Message a
eitherResult (Done _ a) = Right a
eitherResult (Fail _ e) = Left e

// parse :: (Parser a) String -> Result a
parse (Parser p) s :== p s

////////////////////////////////////////////////////////////////////////////////
/// # Instances
////////////////////////////////////////////////////////////////////////////////

instance Functor Parser where
    fmap f p = Parser run
    where
        run s = case parse p s of
            Done s` a -> Done s` (f a)
            Fail s` e -> Fail s` e

instance Applicative Parser where
    pure a = Parser (\s -> Done s a)

    (<*>) p q = Parser run
    where
        run s = case parse p s of
            Done s` f -> case parse q s` of
                Done s`` a -> Done s`` (f a)
                Fail s`` e -> Fail s`` e
            Fail s` e -> Fail s` e

instance Alternative Parser where
    empty = Parser (\s -> Fail s "")

    (<|>) p q = Parser run
    where
        run s = case parse p s of
            Fail _ _ -> parse q s // Backtracking!
            done -> done

////////////////////////////////////////////////////////////////////////////////
/// # Individual characters
////////////////////////////////////////////////////////////////////////////////

satisfy :: (Char -> Bool) -> Parser Char
satisfy t = Parser run
where
    run s = case 'Slice'.uncons s of
        Just (c`,s`)
            | t c` -> Done s` c`
            | otherwise -> Fail s "satisfy: no match"
        Nothing -> Fail s "satisfy: empty string"

char :: Char -> Parser Char
char c = satisfy ((==) c)

notChar :: Char -> Parser Char
notChar c = satisfy ((<>) c)

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Char
digit = satisfy isDigit

////////////////////////////////////////////////////////////////////////////////
/// # Special characters
////////////////////////////////////////////////////////////////////////////////

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

isVerticalSpace :: Char -> Bool
isVerticalSpace c = '\n' <= c && c <= '\r' //EQUALS c == '\n' || c == '\v' || c == '\f' || c == '\r'

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

space :: Parser Char
space = satisfy isSpace

horizontalSpace :: Parser Char
horizontalSpace = satisfy isHorizontalSpace

verticalSpace :: Parser Char
verticalSpace = satisfy isVerticalSpace

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n")

restOfLine :: Parser ()
restOfLine = skipTill isEndOfLine *> endOfLine

endOfInput :: Parser ()
endOfInput = Parser (\s ->
    'Slice'.isEmpty s
        ? Done s ()
        $ Fail s "endOfInput: there is more")

// endOfInput :: Parser ()
// endOfInput = Parser $ \s ->
//     | 'Slice'.isEmpty s = Done s ()
//     = Fail s "endOfInput: there is more"

////////////////////////////////////////////////////////////////////////////////
/// # String parsing
////////////////////////////////////////////////////////////////////////////////

string :: String -> Parser String
string p = Parser (\s -> 'Slice'.isPrefixOf p s ?
    Done ('Slice'.drop ('String'.length p) s) p $
    Fail s "string: no match")

/// Not in Attoparsec
word :: Parser String
word = takeWhile isAlphanum

/// Not in Attoparsec
between :: Char Char -> Parser String
between o c = char o *> takeTill ((==) c) <* char c

/// ## Given number

take :: Int -> Parser String
take n = Parser (\s ->
    let (p,s`) = 'Slice'.splitAt n s in
        Done s` ('Slice'.unwrap p)) // Never fails!

skip :: Int -> Parser ()
skip n = Parser (\s -> Done ('Slice'.drop n s) ()) // Never fails!

/// ## While predicate holds

takeWhile :: (Char -> Bool) -> Parser String
takeWhile t = Parser (\s ->
    let (p,s`) = 'Slice'.span t s in
        Done s` ('Slice'.unwrap p)) // Never fails!

// takeWhile1 :: (Char -> Bool) -> Parser String
// takeWhile1 t = Parser (\s -> case 'Slice'.span t s of
//     (p,s`)
//         | 'Slice'.isEmpty p -> Fail s "takeWhile1: no match"
//         | otherwise -> Done s` ('Slice'.unwrap p))

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 t = Parser (\s ->
    let (p,s`) = 'Slice'.span t s in
        'Slice'.isEmpty p
            ? Fail s "takeWhile1: no match"
            $ Done s` ('Slice'.unwrap p))

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile t = Parser (\s -> Done ('Slice'.dropWhile t s) ()) // Never fails!

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 t = void (takeWhile1 t)

/// ## Till predicate holds

takeTill :: (Char -> Bool) -> Parser String
takeTill t = takeWhile (not o t) // Never fails!

takeTill1 :: (Char -> Bool) -> Parser String
takeTill1 t = takeWhile1 (not o t)

skipTill :: (Char -> Bool) -> Parser ()
skipTill t = skipWhile (not o t) // Never fails!

skipTill1 :: (Char -> Bool) -> Parser ()
skipTill1 t = skipWhile1 (not o t)

/// ## Spaces

skipSpace :: Parser () //FIXME maybeSkipSpace?
skipSpace = skipWhile isSpace

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

skipVerticalSpace :: Parser ()
skipVerticalSpace = skipWhile isVerticalSpace

skipSpace1 :: Parser ()
skipSpace1 = skipWhile1 isSpace

skipHorizontalSpace1 :: Parser ()
skipHorizontalSpace1 = skipWhile1 isHorizontalSpace

skipVerticalSpace1 :: Parser ()
skipVerticalSpace1 = skipWhile1 isVerticalSpace

////////////////////////////////////////////////////////////////////////////////
/// # Number parsing
////////////////////////////////////////////////////////////////////////////////

/// ## Digit interpretation

decodeWithBase :: !Int !Int !Char -> Int
decodeWithBase base last char = last * base + (toInt char - 48)

decimal :: Parser Int
decimal = 'String'.foldl` (decodeWithBase 10) 0 <$> takeWhile1 isDigit

hexadecimal :: Parser Int
hexadecimal = 'String'.foldl` (decodeWithBase 16) 0 <$> takeWhile1 isHexDigit

octal :: Parser Int
octal = 'String'.foldl` (decodeWithBase 8) 0 <$> takeWhile1 isOctDigit

scientific :: Parser (Int, Int)
scientific = go <$> signed decimal <*> option "" fraction <*> option 0 exponent
where
    fraction = char '.' *> takeWhile1 isDigit
    exponent = (char 'e' <|> char 'E') *> signed decimal
    go coefficient digits exponent
        # coefficient = 'String'.foldl` (decodeWithBase 10) coefficient digits
        # exponent = exponent - size digits
        = (coefficient, exponent)

signed :: (Parser a) -> Parser a | ~ a
signed p =
    (~) <$> (char '-' *> p)
    <|> char '+' *> p
    <|> p

/// ## Convenience

nat :: Parser Int //FIXME should be Nat
nat = decimal

int :: Parser Int
int = signed decimal

real :: Parser Real
real = go <$> scientific
where
    go (coefficient, exponent) = toReal coefficient * 10.0 ^ toReal exponent

////////////////////////////////////////////////////////////////////////////////
/// # Combinators
////////////////////////////////////////////////////////////////////////////////

choice :: [f a] -> f a | Alternative f
// choice :: (t (f a)) -> f a | Traversable t, Applicative f
choice xs = 'List'.foldr (<|>) empty xs

count :: !Int (f a) -> f [a] | Alternative f
count n f
    = sequenceA ('List'.replicate n f)
    // | n <= 0 = pure [] // Optimalization
    // | otherwise = sequence ('List'.replicate n f)

option :: a (f a) -> f a | Alternative f
option x f = f <|> pure x

many1 :: (f a) -> f [a] | Alternative f
many1 f = some f

// manyTill :: (Parser a) (Parser b) -> Parser [a]
manyTill :: (f a) (f b) -> f [a] | Alternative f
manyTill p end = scan
        where scan = (end *> pure []) <|> (\x xs -> [x:xs]) <$> p <*> scan

// sepBy :: (Parser a) (Parser s) -> Parser [a]
// sepBy :: (f a) (f s) -> f [a] | Alternative f

// sepBy1 :: (Parser a) (Parser s) -> Parser [a]
sepBy1 :: (f a) (f s) -> f [a] | Alternative f
sepBy1 p s = scan
    where scan = (\x xs -> [x:xs]) <$> p <*> ((s *> scan) <|> pure [])

/// Like `many`, but skip zero or more instances of an action.
skipMany :: (f a) -> f () | Alternative f
skipMany p = scan
    where scan = (p *> scan) <|> pure ()

/// Like `many1`, but skip one or more instances of an action.
skipMany1 :: (f a) -> f () | Alternative f
skipMany1 p = p *> skipMany p
