definition module Yard

// Taken from https://github.com/dopefishh/cc1516/blob/master/yard.dcl
// To this file, an other license applies:

/*
The MIT License (MIT)

Copyright (c) 2016 Pim Jager & Mart Lubbers

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

from StdString import class toString
from Data.Either import :: Either
from StdClass import class ==, class Eq
from Data.Functor import class Functor
from Control.Monad import class Monad
from Control.Applicative import class Applicative, class Alternative

:: Error = PositionalError Int Int String | Error String
:: Parser a b = Parser ([a] -> (Either Error b, [a]))

instance Functor (Parser a)
instance Applicative (Parser a) 
instance Monad (Parser a)
instance Alternative (Parser a)

instance toString Error

runParser :: (Parser a b) [a] -> (Either Error b, [a])
(<?>) :: (Parser a b) Error -> Parser a b
fail :: Parser a b
top :: Parser a a
peek :: Parser a a
satisfy :: (a -> Bool) -> Parser a a
check :: (a -> Bool) -> Parser a a
(until) infix 2 :: (Parser a b) (Parser a c) -> Parser a [b]
item :: a -> Parser a a | Eq a
list :: [a] -> Parser a [a] | Eq a
eof :: Parser a ()
