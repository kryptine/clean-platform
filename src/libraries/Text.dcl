definition module Text
/**
* This module defines the basic operations on pieces of text
* It also gives an implementation for the String type which
* is assumed to be an ASCII string
*/

/**
* This class defines the basic operations for manipulating pieces of text.
*/
class Text s 
	where
	/**
	* Calculates the number of logical characters in a piece of text.
	* When a multibyte encoding is used, this is less then the size in bytes.
	*/
	textSize			:: !s -> Int
	/**
	* Splits a string into a list of strings using a separator string.
	*
	* @param The separator string.
	* @param The string that is to be splitted.
	*/
	split				:: !s !s -> [s]
	/**
	* Joins a list of strings using a separator string.
	*
	* @param The separator string.
	* @param The string that is to be splitted.
	*/
	join				:: !s ![s] -> s
	/**
	* Find the first occurence of a substring in another string.
	*
	* @param The search string.
	* @param The string that is being searched.
	*/
	indexOf 			:: !s !s -> Int
	/**
	* Find the last occurence of a substring in another string.
	*
	* @param The search string.
	* @param The string that is being searched.
	*/
	lastIndexOf 		:: !s !s -> Int
	/**
	* Predicate which tests if a string starts with another substring
	*
	* @param The substring.
	* @param The string that is being searched.
	*/
	startsWith			:: !s !s -> Bool
	/**
	* Predicate which tests if a string ends with another substring
	*
	* @param The substring.
	* @param The string that is being searched.
	*/
	endsWith			:: !s !s -> Bool
	/**
	* Take a substring from a string
	*
	* @param the logical start index.
	* @param the logical length of the substring.
	* @param the string from which the substring is taken.
	*/
	subString			:: !Int !Int !s -> s 
	/**
	* Replaces all occurences of a substring with another in a string
	*
	* @param The substring.
	* @param The replacement.
	* @param The string that is being searched.
	*/
	replaceSubString	:: !s !s !s -> s
	/**
	* Removes whitespace from the beginning and end of a string.
	*/
	trim				:: !s -> s
	/**
	* Removes whitespace from the beginning of a string.
	*/
	ltrim				:: !s -> s
	/**
	* Removes whitespace from the end of a string.
	*/
	rtrim				:: !s -> s
	/**
	* Converts all characters in a string to lower case.
	*/
	toLowerCase			:: !s -> s
	/**
	* Converts all characters in a string to upper case.
	*/
	toUpperCase			:: !s -> s

/**
* Instances of the text operations for plain ASCII strings.
*/
instance Text String