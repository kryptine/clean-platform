implementation module Text.Unicode.UCharImpl

eq_int :: !Int !Int -> Bool
eq_int a b = code inline {
			eqI
	}

lt_int :: !Int !Int -> Bool
lt_int a b = code inline {
			ltI
	}

to_int_from_char :: !Char -> Int
to_int_from_char a = code inline {
			CtoI
	}

to_char_from_int :: !Char -> Int
to_char_from_int a = code inline {
			ItoC
	}