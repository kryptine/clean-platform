implementation module _Pointer

import StdOverloaded, StdClass, StdArray, StdInt, StdChar, StdString
/*
   0:   8b 04 18                mov    (%eax,%ebx,1),%eax
   3:   0f b7 04 18             movzwl (%eax,%ebx,1),%eax
   7:   0f b6 04 18             movzbl (%eax,%ebx,1),%eax
   b:   0f bf 04 18             movswl (%eax,%ebx,1),%eax
   f:   0f be 04 18             movsbl (%eax,%ebx,1),%eax
  13:   dd 04 18                fldl   (%eax,%ebx,1)
  16:   dd d9                   fstp   %st(1)
  18:   d9 04 18                flds   (%eax,%ebx,1)
  1b:   dd d9                   fstp   %st(1)
  1d:   dd 14 18                fstl   (%eax,%ebx,1)
  20:   d9 14 18                fsts   (%eax,%ebx,1)
  23:   89 d8                   mov    %ebx,%eax
  25:   59                      pop    %ecx
  26:   89 0c 18                mov    %ecx,(%eax,%ebx,1)
  29:   66 89 0c 18             mov    %cx,(%eax,%ebx,1)
  2d:   88 0c 18                mov    %cl,(%eax,%ebx,1)
  30:   8b 0c 24                mov    (%esp),%ecx
*/

/*
 00: 48 63 04 03        movsxd      rax,dword ptr [rbx+rax]
 04: F2 0F 10 04 03     movsd       xmm0,mmword ptr [rbx+rax]
 09: F3 0F 10 04 03     movss       xmm0,dword ptr [rbx+rax]
 0E: F3 0F 5A C0        cvtss2sd    xmm0,xmm0
 12: F2 0F 5A C0        cvtsd2ss    xmm0,xmm0
 16: F3 0F 5A 04 03     cvtss2sd    xmm0,dword ptr [rbx+rax]
 1B: F2 0F 11 04 03     movsd       mmword ptr [rbx+rax],xmm0
 20: F3 0F 11 04 03     movss       dword ptr [rbx+rax],xmm0
 25: 4A 89 04 13        mov         qword ptr [rbx+r10],rax
 29: 42 89 04 13        mov         dword ptr [rbx+r10],eax
 2D: 66 42 89 04 13     mov         word ptr [rbx+r10],ax
 32: 42 88 04 13        mov         byte ptr [rbx+r10],al
*/

readInt :: !Pointer !Offset -> Int
readInt pointer offset = IF_INT_64_OR_32 (readInt_64 pointer offset) (readInt_32 pointer offset)

readInt_64 :: !Pointer !Offset -> Int
readInt_64 pointer offset = code {
		pop_b 1
|		mov    (%rax,%rbx,1),%rax
		instruction 72
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readInt_32 :: !Pointer !Offset -> Int
readInt_32 pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readInt4Z :: !Pointer !Offset -> Int
readInt4Z pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readInt4S :: !Pointer !Offset -> Int
readInt4S pointer offset = IF_INT_64_OR_32 (readInt4S_64 pointer offset) (readInt4S_32 pointer offset)

readInt4S_64 :: !Pointer !Offset -> Int
readInt4S_64 pointer offset = code {
		pop_b 1
|		movsxd rax,dword ptr [rbx+rax]
		instruction 72
	 	instruction 99
	 	instruction 4
	 	instruction 3
}

readInt4S_32 :: !Pointer !Offset -> Int
readInt4S_32 pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readInt2Z :: !Pointer !Offset -> Int
readInt2Z pointer offset = code {
		pop_b 1
|		movzwl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 183
	 	instruction 4
	 	instruction 24
}

readInt2S :: !Pointer !Offset -> Int
readInt2S pointer offset = IF_INT_64_OR_32 (readInt2S_64 pointer offset) (readInt2S_32 pointer offset)

readInt2S_64 :: !Pointer !Offset -> Int
readInt2S_64 pointer offset = code {
		pop_b 1
|		movswl (%rax,%rbx,1),%rax
		instruction 72
	 	instruction 15
	 	instruction 191
	 	instruction 4
	 	instruction 24
}

readInt2S_32 :: !Pointer !Offset -> Int
readInt2S_32 pointer offset = code {
		pop_b 1
|		movswl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 191
	 	instruction 4
	 	instruction 24
}

readInt1Z :: !Pointer !Offset -> Int
readInt1Z pointer offset = code {
		pop_b 1
|		movzbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 182
	 	instruction 4
	 	instruction 24
}

readInt1S :: !Pointer !Offset -> Int
readInt1S pointer offset = IF_INT_64_OR_32 (readInt1S_64 pointer offset) (readInt1S_32 pointer offset)

readInt1S_64 :: !Pointer !Offset -> Int
readInt1S_64 pointer offset = code {
		pop_b 1
|		movsbl (%rax,%rbx,1),%rax
		instruction 72
	 	instruction 15
	 	instruction 190
	 	instruction 4
	 	instruction 24
}

readInt1S_32 :: !Pointer !Offset -> Int
readInt1S_32 pointer offset = code {
		pop_b 1
|		movsbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 190
	 	instruction 4
	 	instruction 24
}

readChar :: !Pointer !Offset -> Char
readChar pointer offset = code {
		pop_b 1
|		movzbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 182
	 	instruction 4
	 	instruction 24
}

readReal8 :: !Pointer !Offset -> Real
readReal8 pointer offset = IF_INT_64_OR_32 (readReal8_64 pointer offset) (readReal8_32 pointer offset)

readReal8_64 :: !Pointer !Offset -> Real
readReal8_64 pointer offset = code {
		pushR 0.0
		updatepop_b 0 2
		jmp read_f8_p_64
		:read_f8_p_64
|		movsd xmm0,mmword ptr [rbx+rax]
		instruction 242
		instruction 15
		instruction 16
		instruction 4
		instruction 3
}

readReal8_32 :: !Pointer !Offset -> Real
readReal8_32 pointer offset = code {
		pushR 0.0
		update_b 1 3
		updatepop_b 0 2
		jmp read_f8_p_32
		:read_f8_p_32
|		fldl   (%eax,%ebx,1)
		instruction 221
		instruction 4
		instruction 24
|		fstp   %st(1)
		instruction 221
		instruction 217
}

readReal4 :: !Pointer !Offset -> Real
readReal4 pointer offset = IF_INT_64_OR_32 (readReal4_64 pointer offset) (readReal4_32 pointer offset)

readReal4_64 :: !Pointer !Offset -> Real
readReal4_64 pointer offset = code {
		pushR 0.0
		updatepop_b 0 2
		jmp read_f4_p_64
		:read_f4_p_64
|		cvtss2sd    xmm0,dword ptr [rbx+rax]
		instruction 243
		instruction 15
		instruction 90
		instruction 4
		instruction 3
}

readReal4_32 :: !Pointer !Offset -> Real
readReal4_32 pointer offset = code {
		pushR 0.0
		update_b 1 3
		updatepop_b 0 2
		jmp read_f4_p_32
		:read_f4_p_32
|		flds   (%eax,%ebx,1)
		instruction 217
		instruction 4
		instruction 24
|		fstp   %st(1)
		instruction 221
		instruction 217
}

writeInt :: !Pointer !Offset !Int -> Int
writeInt pointer offset i = IF_INT_64_OR_32 (writeInt_64 pointer offset i) (writeInt_32 pointer offset i)

writeInt_64 :: !Pointer !Offset !Int -> Int
writeInt_64 pointer offset i = code {
		updatepop_b 0 2
|		mov qword ptr [rbx+r10],rax
		instruction 74
		instruction 137
		instruction 4
		instruction 19
}

writeInt_32 :: !Pointer !Offset !Int -> Int
writeInt_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%ecx,(%eax,%ebx,1)
		instruction 137
		instruction 12
		instruction 24
}

writeInt4 :: !Pointer !Offset !Int -> Int
writeInt4 pointer offset i = IF_INT_64_OR_32 (writeInt4_64 pointer offset i) (writeInt4_32 pointer offset i)

writeInt4_64 :: !Pointer !Offset !Int -> Int
writeInt4_64 pointer offset i = code {
		updatepop_b 0 2
|		mov dword ptr [rbx+r10],eax
 		instruction 66
		instruction 137
		instruction 4
		instruction 19
}

writeInt4_32 :: !Pointer !Offset !Int -> Int
writeInt4_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%ecx,(%eax,%ebx,1)
		instruction 137
		instruction 12
		instruction 24
}

writeInt2 :: !Pointer !Offset !Int -> Int
writeInt2 pointer offset i = IF_INT_64_OR_32 (writeInt2_64 pointer offset i) (writeInt2_32 pointer offset i)

writeInt2_64 :: !Pointer !Offset !Int -> Int
writeInt2_64 pointer offset i = code {
		updatepop_b 0 2
|		mov word ptr [rbx+r10],ax
		instruction 102
 		instruction 66
		instruction 137
		instruction 4
		instruction 19
}

writeInt2_32 :: !Pointer !Offset !Int -> Int
writeInt2_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movw	%cx,(%eax,%ebx,1)
		instruction 102
		instruction 137
		instruction 12
		instruction 24
}

writeInt1 :: !Pointer !Offset !Int -> Int
writeInt1 pointer offset i = IF_INT_64_OR_32 (writeInt1_64 pointer offset i) (writeInt1_32 pointer offset i)

writeInt1_64 :: !Pointer !Offset !Int -> Int
writeInt1_64 pointer offset i = code {
		updatepop_b 0 2
|		mov byte ptr [rbx+r10],al
		instruction 66
		instruction 136
		instruction 4
		instruction 19
}

writeInt1_32 :: !Pointer !Offset !Int -> Int
writeInt1_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%cl,(%eax,%ebx,1)
		instruction 136
		instruction 12
		instruction 24
}

writeChar :: !Pointer !Offset !Char -> Int
writeChar pointer offset i = IF_INT_64_OR_32 (writeChar_64 pointer offset i) (writeChar_32 pointer offset i)

writeChar_64 :: !Pointer !Offset !Char -> Int
writeChar_64 pointer offset i = code {
		updatepop_b 0 2
|		mov byte ptr [rbx+r10],al
		instruction 66
		instruction 136
		instruction 4
		instruction 19
}

writeChar_32 :: !Pointer !Offset !Char -> Int
writeChar_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%cl,(%eax,%ebx,1)
		instruction 136
		instruction 12
		instruction 24
}

writeReal8 :: !Pointer !Offset !Real -> Int
writeReal8 pointer offset double = IF_INT_64_OR_32 (writeReal8_64 pointer offset double) (writeReal8_32 pointer offset double)

writeReal8_64 :: !Pointer !Offset !Real -> Int
writeReal8_64 pointer offset double = code {
		updatepop_b 0 2
|		movsd mmword ptr [rbx+rax],xmm0
		instruction 242
		instruction 15
		instruction 17
		instruction 4
		instruction 3
}

writeReal8_32 :: !Pointer !Offset !Real -> Int
writeReal8_32 pointer offset double = code {
		updatepop_b 0 3
|		fstl  (%eax,%ebx,1)
		instruction	221
		instruction 20
		instruction 24
}

writeReal4 :: !Pointer !Offset !Real -> Int
writeReal4 pointer offset double = IF_INT_64_OR_32 (writeReal4_64 pointer offset double) (writeReal4_32 pointer offset double)

writeReal4_64 :: !Pointer !Offset !Real -> Int
writeReal4_64 pointer offset double = code {
		updatepop_b 0 2
|		cvtsd2ss xmm0,xmm0
		instruction 242
		instruction 15
		instruction 90
		instruction 192
|		movss dword ptr [rbx+rax],xmm0
		instruction 243
		instruction 15
		instruction 17
		instruction 4
		instruction 3
}

writeReal4_32 :: !Pointer !Offset !Real -> Int
writeReal4_32 pointer offset double = code {
		updatepop_b 0 3
|		fsts  (%eax,%ebx,1)
		instruction	217
		instruction 20
		instruction 24
}

derefInt :: !Pointer -> Int
derefInt ptr = code {
		load_i 0
}

derefString :: !Pointer -> String
derefString ptr = copy ptr 0 (createArray len '\0')
where
	len = skip_to_zero ptr - ptr

	skip_to_zero ptr
		| load_char ptr <> '\0'	= skip_to_zero (ptr+1)
								= ptr

	copy :: !Pointer !Offset *{#Char} -> *{#Char}
	copy ptr off arr
		# char = load_char (ptr+off)
		| char <> '\0'	= copy ptr (off + 1) {arr & [off] = char}
						= arr
	
derefCharArray :: !Pointer !Int -> {#Char}
derefCharArray ptr len = copy 0 (createArray len '\0')
where
	copy :: !Offset *{#Char} -> *{#Char}
	copy off arr
		# char = load_char (ptr+off)
		| off < len	= copy (inc off) {arr & [off] = char}
					= arr

load_char :: !Pointer -> Char
load_char ptr = code inline {
		load_ui8 0
	}
	
writeCharArray :: !Pointer !{#Char} -> Int
writeCharArray ptr array = copy ptr 0
where
	len = size array
	
	copy :: !Pointer !Offset -> Pointer
	copy ptr off
		# char = array.[off]
		| off < len	= copy (writeChar ptr off char) (inc off)
					= ptr

packInt :: !Int -> {#Int}
packInt i = {i}

packString :: !String -> {#Char}
packString s = s +++ "\0"

unpackString :: !{#Char} -> String
unpackString s = unpack 0
where
	unpack :: Int -> String
	unpack off	| s.[off] == '\0' = s % (0, off - 1)
				| otherwise       = unpack (off + 1)

forceEval :: !a !*st -> *st
forceEval _ st = st