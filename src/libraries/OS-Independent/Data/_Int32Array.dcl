definition module Data._Int32Array

from _SystemArray import class Array

:: Int32Array_ a =: Int32Array_ () // actually {#INT32}, but INT32 only exists on ABC level
:: Int32Array :== Int32Array_ Int

instance Array Int32Array_ Int
where
	uselect :: !u:(Int32Array_ Int) !Int -> *(!Int, !u:(Int32Array_ Int))
	update :: !*(Int32Array_ e:Int) !Int !e:Int -> *Int32Array_ e:Int
	createArray :: !Int !Int -> *Int32Array_ Int
	replace :: !*(Int32Array_ e:Int) !Int !e:Int -> *(!e:Int, !*Int32Array_ e:Int)
