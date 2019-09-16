definition module Data.Encoding.GenBinary

/**
 * This module provides a compact binary encoding for arbitrary values.
 * The encoding is provided as character array.
 * Choices of ADTs are represented by a single bit.
 * Values of basic types (except `Bool`), arrays and lists are stored byte-aligned, which wastes only little space,
 * but significantly improves encoding and decoding time.
 *
 * @property-bootstrap
 *     import StdEnv, Data.Maybe.Gast, Data.Maybe.GenPrint, Data.Maybe.GenBinary
 *
 *     :: ADT = A String | B ADT | C ADT ADT | D ADT ADT ADT
 *
 *     derive gEq             ADT
 *     derive class GenBinary ADT
 *     derive class Gast      ADT
 *
 *     instance == ADT where
 *         == x y = x === y
 *
 *     :: Record = {a :: ADT, b :: ADT , c :: ADT}
 *
 *     derive gEq             Record
 *     derive class GenBinary Record
 *     derive class Gast      Record
 *
 *     instance == Record where
 *         == x y = x === y
 *
 * @property-test-with a = Maybe Bool
 * @property-test-with a = Int
 * @property-test-with a = String
 * @property-test-with a = Char
 * @property-test-with a = Real
 * @property-test-with a = (Int, Int)
 * @property-test-with a = (String, String)
 * @property-test-with a = (String, Int)
 * @property-test-with a = (Int, String)
 * @property-test-with a = [Bool]
 * @property-test-with a = [Int]
 * @property-test-with a = [String]
 * @property-test-with a = [Char]
 * @property-test-with a = [Real]
 * @property-test-with a = ADT
 * @property-test-with a = Record
 */

from StdGeneric   import :: UNIT (..), :: PAIR (..), :: EITHER (..), :: CONS (..), :: OBJECT (..), :: RECORD (..),
                         :: FIELD (..)
from StdInt       import class + (+), instance + Int
from Data.Maybe   import :: Maybe (..), instance Functor Maybe
from Data.Func    import $
from Data.Functor import class Functor (fmap)
from Data.Tuple   import appFst

/**
 * Encodes a values as character array.
 *
 * @param The value.
 * @result The encoded value.
 */
encode :: !a -> {#Char} | gBinaryEncodingSize{|*|}, gBinaryEncode{|*|} a

/**
 * Decodes a value.
 *
 * @param The value encoded as character array.
 * @result The corresponding value, if the provided array is a valid representation of a value.
 *
 * @property correctness: A.a :: a:
 *     // The `a == a` check is required as NaN Real values do not equal themselves.
 *     a == a ==> decode (encode a) =.= Just a
 */
decode :: !{#Char} -> Maybe a | gBinaryDecode{|*|} a

class GenBinary a | gBinaryEncode{|*|}, gBinaryEncodingSize{|*|}, gBinaryDecode{|*|} a

:: *EncodingSt

generic gBinaryEncode a :: !a !*EncodingSt -> *EncodingSt
gBinaryEncode{|UNIT|} _ st = st
gBinaryEncode{|PAIR|} cx cy (PAIR x y) st = cy y $ cx x st
gBinaryEncode{|EITHER|} cl cr (LEFT x) st = cl x $ encodeBool False st
gBinaryEncode{|EITHER|} cl cr (RIGHT x) st = cr x $ encodeBool True st
gBinaryEncode{|CONS|} c (CONS x) st = c x st
gBinaryEncode{|FIELD|} c (FIELD x) st = c x st
gBinaryEncode{|OBJECT|} c (OBJECT x) st = c x st
gBinaryEncode{|RECORD|} c (RECORD x) st = c x st

derive gBinaryEncode Int, Real, Bool, Char, String, [], {}, {!}, (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,),
                     (,,,,,,,)

generic gBinaryEncodingSize a :: !a !Int -> Int
gBinaryEncodingSize{|UNIT|} _ s = s
gBinaryEncodingSize{|PAIR|} cx cy (PAIR x y) s = cy y $ cx x s
gBinaryEncodingSize{|EITHER|} cl _ (LEFT x) s = cl x $ s + 1
gBinaryEncodingSize{|EITHER|} _ cr (RIGHT x) s = cr x $ s + 1
gBinaryEncodingSize{|CONS|} c (CONS x) s = c x s
gBinaryEncodingSize{|FIELD|} c (FIELD x) s = c x s
gBinaryEncodingSize{|OBJECT|} c (OBJECT x) s = c x s
gBinaryEncodingSize{|RECORD|} c (RECORD x) s = c x s

derive gBinaryEncodingSize Int, Real, Bool, Char, String, [], {}, {!}, (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,),
                           (,,,,,,,)

generic gBinaryDecode a :: !*EncodingSt -> (!Maybe a, !*EncodingSt)
gBinaryDecode{|UNIT|} st = (Just UNIT, st)
gBinaryDecode{|PAIR|} fx fy st
    # (mbX, st) = fx st
    # (mbY, st) = fy st
    = case (mbX, mbY) of
        (Just x, Just y) = (Just $ PAIR x y, st)
        _                = (Nothing,         st)
gBinaryDecode{|EITHER|} fl fr st
    # (mbIsRight, st) = decodeBool st
    = case mbIsRight of
        Just isRight | isRight   = appFst (fmap RIGHT) $ fr st
                     | otherwise = appFst (fmap LEFT)  $ fl st
        _                        = (Nothing, st)
gBinaryDecode{|CONS|} f st = appFst (fmap CONS) $ f st
gBinaryDecode{|FIELD|} f st = appFst (fmap \x -> FIELD x) $ f st
gBinaryDecode{|OBJECT|} f st = appFst (fmap \x -> OBJECT x) $ f st
gBinaryDecode{|RECORD|} f st = appFst (fmap RECORD) $ f st

derive gBinaryDecode Int, Real, Bool, Char, String, [], {}, {!}, (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,),
                     (,,,,,,,)

// This is only exported because it is used in exposed generic definitions.
decodeBool :: !*EncodingSt -> (!Maybe Bool, !*EncodingSt)
encodeBool :: !Bool !*EncodingSt -> *EncodingSt
