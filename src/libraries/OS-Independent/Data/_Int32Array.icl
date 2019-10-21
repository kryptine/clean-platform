implementation module Data._Int32Array

import StdEnv

instance Array Int32Array_ Int
where
	select _ _ = code {
		select INT32 0 1
	}

	uselect :: !u:(Int32Array_ Int) !Int -> *(!Int, !u:(Int32Array_ Int))
	uselect _ _ = code {
		push_a 0
		select INT32 0 1
	}

	size _ = code {
		push_arraysize INT32 0 1
	}

	usize _ = code {
		push_a 0
		push_arraysize INT32 0 1
	}

	update :: !*(Int32Array_ e:Int) !Int !e:Int -> *Int32Array_ e:Int
	update _ _ _ = code {
		update INT32 0 1
	}

	createArray :: !Int !Int -> *Int32Array_ Int
	createArray _ _ = code {
		create_array INT32 0 1
	}

	_createArray _ = code {
		create_array_ INT32 0 1
	}

	replace :: !*(Int32Array_ e:Int) !Int !e:Int -> *(!e:Int, !*Int32Array_ e:Int)
	replace _ _ _ = code {
		replace INT32 0 1
	}
