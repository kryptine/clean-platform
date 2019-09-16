implementation module Data.Encoding.GenBinary

import StdGeneric, StdEnv
import Data._Array, Data.Func, Data.Maybe, Data.Functor, Data.Tuple, Data.Array
import System._Unsafe

decode :: !{#Char} -> Maybe a | gBinaryDecode{|*|} a
decode binary = fst $ gBinaryDecode{|*|} $ mkEncodingSt {x \\ x <-: binary}

encode :: !a -> {#Char} | gBinaryEncodingSize{|*|}, gBinaryEncode{|*|} a
encode x
	#! encoded_size = gBinaryEncodingSize{|*|} x 0
	#! arr_size = (encoded_size+7) >> 3
	#! bits = createArray arr_size '\0'
	= (gBinaryEncode{|*|} x (mkEncodingSt bits)).cs_bits

mkEncodingSt :: !*{#Char} -> *EncodingSt
mkEncodingSt arr = { cs_pos = 0, cs_bits = arr}

generic gBinaryEncode a :: !a !*EncodingSt -> *EncodingSt
gBinaryEncode{|Int|}    x                st = encodeInt x st
gBinaryEncode{|Real|}   x                st = encodeReal x st
gBinaryEncode{|Char|}   x                st = encodeChar x st
gBinaryEncode{|Bool|}   x                st = encodeBool x st
gBinaryEncode{|String|} xs               st = encodeArray encodeChar xs st
gBinaryEncode{|UNIT|}   _                st = st
gBinaryEncode{|PAIR|}   cx cy (PAIR x y) st = cy y $ cx x st
gBinaryEncode{|EITHER|} cl cr (LEFT x)   st = cl x $ encodeBool False st
gBinaryEncode{|EITHER|} cl cr (RIGHT x)  st = cr x $ encodeBool True st
gBinaryEncode{|CONS|}   c (CONS x)       st = c x st
gBinaryEncode{|FIELD|}  c (FIELD x)      st = c x st
gBinaryEncode{|OBJECT|} c (OBJECT x)     st = c x st
gBinaryEncode{|RECORD|} c (RECORD x)     st = c x st
gBinaryEncode{|{}|}     c xs             st = encodeArray c xs st
gBinaryEncode{|{!}|}    c xs             st = encodeArray c xs st
gBinaryEncode{|[]|}     c xs             st = encodeList c xs st

encodeInt :: !Int !*EncodingSt -> *EncodingSt
encodeInt int st = encodeIntUsingNBytes (IF_INT_64_OR_32 8 4) int st

encodeChar :: !Char !*EncodingSt -> *EncodingSt
encodeChar c st = encodeIntUsingNBytes 1 (toInt c) st

encodeBool :: !Bool !*EncodingSt -> *EncodingSt
encodeBool False st = {st & cs_pos = st.cs_pos + 1}
encodeBool True {cs_pos = pos, cs_bits = bits}
	#! byte_pos = pos >> 3
	#! bit_pos = pos bitand 7
	#! int = toInt bits.[byte_pos]
	#! bit_mask = 1 << bit_pos
	= {cs_pos = inc pos, cs_bits = {bits & [byte_pos] = toChar $ int bitor bit_mask}}

encodeReal :: !Real !*EncodingSt -> *EncodingSt
encodeReal real st = IF_INT_64_OR_32
	(encodeInt (unsafeCoerce real) st)
	(let (i1, i2) = unsafeCoerce real in encodeInt i2 $ encodeInt i1 st)

encodeArray :: !(a *EncodingSt -> *EncodingSt) !(b a) !*EncodingSt -> *EncodingSt | Array b a
encodeArray f xs st
	#! st = encodeInt (size xs) st
	= foldlArr (flip f) st xs

encodeList :: !(a *EncodingSt -> *EncodingSt) ![a] !*EncodingSt -> *EncodingSt
encodeList f xs st
	#! st = encodeInt (length xs) st
	= foldl (flip f) st xs

encodeIntUsingNBytes :: !Int !Int !*EncodingSt -> *EncodingSt
encodeIntUsingNBytes numBytes int st = encode numBytes $ withByteAlignedPosition st
where
	encode :: !Int !*EncodingSt -> *EncodingSt
	encode 0              st = st
	encode remainingBytes st
		#! byte_pos = st.cs_pos >> 3
		#! st =
			{ st
			& cs_bits = {st.cs_bits & [byte_pos] = toChar $ int >> ((numBytes - remainingBytes) * 8)}
			, cs_pos  = st.cs_pos + 8
			}
		= encode (dec remainingBytes) st

generic gBinaryEncodingSize a :: !a !Int -> Int
gBinaryEncodingSize{|Int|}    _                s  = (IF_INT_64_OR_32 64 32) + byteAlignedPosition s
gBinaryEncodingSize{|Real|}   _                s  = 64 + byteAlignedPosition s
gBinaryEncodingSize{|Char|}   _                s  = 8 + byteAlignedPosition s
gBinaryEncodingSize{|Bool|}   _                s = 1 + s
gBinaryEncodingSize{|String|} xs               s = IF_INT_64_OR_32 64 32 + size xs * 8 + byteAlignedPosition s
gBinaryEncodingSize{|UNIT|}   _                s = s
gBinaryEncodingSize{|PAIR|}   cx cy (PAIR x y) s = cy y $ cx x s
gBinaryEncodingSize{|EITHER|} cl _ (LEFT x)    s = cl x $ s + 1
gBinaryEncodingSize{|EITHER|} _ cr (RIGHT x)   s = cr x $ s + 1
gBinaryEncodingSize{|CONS|}   c (CONS x)       s = c x s
gBinaryEncodingSize{|FIELD|}  c (FIELD x)      s = c x s
gBinaryEncodingSize{|OBJECT|} c (OBJECT x)     s = c x s
gBinaryEncodingSize{|RECORD|} c (RECORD x)     s = c x s
gBinaryEncodingSize{|[]|}  c xs s = foldl    (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|{}|}  c xs s = foldlArr (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|{!}|} c xs s = foldlArr (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs

generic gBinaryDecode a :: !*EncodingSt -> (!Maybe a, !*EncodingSt)
gBinaryDecode{|Int|}    st = decodeInt st
gBinaryDecode{|Real|}   st = decodeReal st
gBinaryDecode{|Char|}   st = decodeChar st
gBinaryDecode{|Bool|}   st = decodeBool st
gBinaryDecode{|String|} st = decodeArray decodeChar st
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
		Just isRight
			| isRight   = appFst (fmap RIGHT) $ fr st
			| otherwise = appFst (fmap LEFT)  $ fl st
		_ = (Nothing, st)
gBinaryDecode{|CONS|}   f st = appFst (fmap CONS) $ f st
gBinaryDecode{|FIELD|}  f st = appFst (fmap \x -> FIELD x) $ f st
gBinaryDecode{|OBJECT|} f st = appFst (fmap \x -> OBJECT x) $ f st
gBinaryDecode{|RECORD|} f st = appFst (fmap RECORD) $ f st
gBinaryDecode{|[]|}     f st = decodeList f st
gBinaryDecode{|{}|}     f st = decodeArray f st
gBinaryDecode{|{!}|}    f st = decodeArray f st

decodeInt :: !*EncodingSt -> (!Maybe Int, !*EncodingSt)
decodeInt st = decodeIntWithNBytes (IF_INT_64_OR_32 8 4) st

decodeChar :: !*EncodingSt -> (!Maybe Char, !*EncodingSt)
decodeChar st
	# (mbInt, st) = decodeIntWithNBytes 1 st
	= (toChar <$> mbInt, st)

decodeBool :: !*EncodingSt -> (!Maybe Bool, !*EncodingSt)
decodeBool cs=:{cs_pos = pos, cs_bits = bits}
	#! s = size bits
	#! byte_pos = pos >> 3
	#! bit_pos = pos bitand 7
	| s == byte_pos = (Nothing, cs)
	#! int = toInt bits.[byte_pos]
	#! bit_mask = 1 << bit_pos
	#! bit = (bit_mask bitand int) <> 0
	= (Just bit, {cs & cs_pos = inc pos})

decodeReal :: !*EncodingSt -> (!Maybe Real, !*EncodingSt)
decodeReal st = IF_INT_64_OR_32 decodeReal64 decodeReal32 $ st
where
	decodeReal64 st
		# (mbInt, st) = decodeInt st
		= (unsafeCoerce <$> mbInt, st)

	decodeReal32 st
		# (mbInt1, st) = decodeInt st
		# (mbInt2, st) = decodeInt st
		= case (mbInt1, mbInt2) of
			(Just int1, Just int2) = (Just $ unsafeCoerce (int1, int2), st)
			_                      = (Nothing, st)

decodeArray :: !(*EncodingSt -> (Maybe a, *EncodingSt)) !*EncodingSt -> (!Maybe (b a), !*EncodingSt) | Array b a
decodeArray f st
	# (mbLength, st) = decodeInt st
	= case mbLength of
		Just l = decodeArray 0 l (unsafeCreateArray l) st
		_      = (Nothing, st)
where
	decodeArray i s arr st
		| i == s = (Just arr, st)
		| otherwise
			# (mbX, st) = f st
			= case mbX of
				Just x = decodeArray (inc i) s {arr & [i] = x} st
				_      = (Nothing, st)

decodeList :: !(*EncodingSt -> (Maybe a, *EncodingSt)) !*EncodingSt -> (!Maybe [a], !*EncodingSt)
decodeList xs st
	# (mbArr, st) = decodeArray xs st
	= (arrToList <$> mbArr, st)
where
	arrToList :: !{b} -> [b]
	arrToList xs = [x \\ x <-: xs]

decodeIntWithNBytes :: !Int !*EncodingSt -> (!Maybe Int, !*EncodingSt)
decodeIntWithNBytes numBytes st=:{cs_pos} = decode numBytes 0 $ withByteAlignedPosition st
where
	// we can decode an entire byte at once, as the start position is byte-aligned
	decode :: !Int !Int !*EncodingSt -> (!Maybe Int, !*EncodingSt)
	decode 0              int st = (Just int, st)
	decode remainingBytes int st=:{cs_bits}
		#! byte_pos = st.cs_pos >> 3
		| byte_pos == size cs_bits = (Nothing, st)
		#! byte = toInt cs_bits.[byte_pos]
		= decode (dec remainingBytes) (byte << ((numBytes - remainingBytes) * 8) + int) {st & cs_pos = st.cs_pos + 8}

withByteAlignedPosition :: !*EncodingSt -> *EncodingSt
withByteAlignedPosition st=:{cs_pos} = {st & cs_pos = byteAlignedPosition cs_pos}

byteAlignedPosition :: !Int -> Int
byteAlignedPosition pos = (pos + 7) bitand -8

:: *EncodingSt = {cs_pos :: !Int, cs_bits :: !*{#Char}}

derive gBinaryEncode       (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive gBinaryEncodingSize (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive gBinaryDecode       (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
