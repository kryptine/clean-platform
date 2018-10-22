implementation module Data._Array

import StdEnv

unsafeCreateArray :: .Int -> u:(a v:b) | Array a b, [u<=v]
unsafeCreateArray size = code {
		updatepop_a 0 7
		jmp_ap 1
	}

unsafeUselect :: u:(a v:b) Int -> *(v:b,u:(a v:b)) | Array a b, [u<=v]
unsafeUselect arr index = code {
		push_a 0
		select _ 1 0
	}
