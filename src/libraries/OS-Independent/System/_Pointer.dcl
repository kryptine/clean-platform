definition module _Pointer
/**
* Low level reading from and writing to memory using pointers and offsets.
*
* WARNING:
* This module provides unsafe and impure functions that can really mess up
* your program when used incorrectly.
* Only use these if you understand the risks of these low-level operations.
*/

:: Pointer	:== Int
:: Offset	:== Int

/**
* Read an integer (32 or 64 bits)
*/
readInt		:: !Pointer !Offset -> Int
/**
* Read an integer (32 bits) zero extended
*/
readInt4Z	:: !Pointer !Offset -> Int
/**
* Read an integer (32 bits) sign extended
*/
readInt4S	:: !Pointer !Offset -> Int
/**
* Read a word (16 bits) zero extended
*/
readInt2Z	:: !Pointer !Offset -> Int
/**
* Read a word (16 bits) sign extended
*/
readInt2S	:: !Pointer !Offset -> Int
/**
* Read a byte (8 bits) zero extended
*/
readInt1Z	:: !Pointer !Offset -> Int
/**
* Read a byte (8 bits) sign extended
*/
readInt1S	:: !Pointer !Offset -> Int
/**
* Read a char
*/
readChar	:: !Pointer !Offset -> Char
/**
* Read a real (8 bytes)
*/
readReal8	:: !Pointer !Offset -> Real
/**
* Read a real (4 bytes)
*/
readReal4	:: !Pointer !Offset -> Real

/**
* Write an integer (32 or 64 bits)
*/
writeInt	:: !Pointer !Offset !Int -> Int
/**
* Write an integer (32 bits)
*/
writeInt4	:: !Pointer !Offset !Int -> Int
/**
* Write a word (16 bits)
*/
writeInt2	:: !Pointer !Offset !Int -> Int
/**
* Write a word (8 bits)
*/
writeInt1	:: !Pointer !Offset !Int -> Int
/**
* Write a char 
*/
writeChar	:: !Pointer !Offset !Char -> Int
/**
* Write a real (8 bytes) 
*/
writeReal8	:: !Pointer !Offset !Real -> Int
/**
* Write a real (4 bytes)
*/
writeReal4	:: !Pointer !Offset !Real -> Int

//Utility functions

/**
* Reads the integer located at the pointer
*/
derefInt :: !Pointer -> Int
/**
* Reads the NULL-terminated C-string indicated by the pointer and
* converts it to a normal (not NULL-terminated) Clean-string
*/
derefString :: !Pointer -> String
/**
* Wraps an integer in an array to enable passing a pointer instead
* of a value to a ccall.
*/
packInt :: !Int -> {#Int}
/**
* Wraps a Clean-string as a NULL-terminated C-string to enable passing
* a pointer to a ccall using the C conventions.
*/
packString :: !String -> {#Char}

/**
* Unpacks a NULL-terminated C-string into a Clean-string.
*/
unpackString :: !{#Char} -> String