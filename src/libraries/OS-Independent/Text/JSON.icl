implementation module Text.JSON

import StdGeneric, Data.Maybe, StdList, StdOrdList, StdString, _SystemArray, StdTuple, StdBool, StdFunc, StdOverloadedList, StdFile
import Text, Text.PPrint

//Basic JSON serialization
instance toString JSONNode
where
	//make target string -> copy characters
	//The reason why first a big string is made into which the characters are copied is to
	//avoid many string concatenations with big strings
	toString node
		#! len = sizeOf node
		= snd (copyNode 0 node (createArray len '\0'))

//Determine serialized size of a JSON datastructure
sizeOf :: !JSONNode -> Int
sizeOf (JSONNull)       = 4
sizeOf (JSONBool True)  = 4
sizeOf (JSONBool False) = 5
sizeOf (JSONInt x)      = size (toString x)
sizeOf (JSONReal x)     = size (toString x)
sizeOf (JSONString x)   = size x + 2 + count_escape_chars 0 x
sizeOf (JSONArray x)
  #! len = length x
  = (if (len > 0) (foldl (\s x -> s + sizeOf x) (len - 1) x) 0) + 2
sizeOf (JSONObject x)
  #! len = length x
  = (if (len > 0) (foldl (\s (l,o) -> s + size l + 2 + 1 + sizeOf o) (len - 1) x) 0) + 2
sizeOf (JSONRaw x)      = size x
sizeOf (JSONError)      = 0

count_escape_chars :: !Int !String -> Int
count_escape_chars i s
	| i < size s
		#! c = s.[i]
		| c >= '0'
			| c <> '\\'
				= count_escape_chars (i + 1) s
				= count_more_escape_chars (i + 1) s 1
			| isControl c = count_more_escape_chars (i + 1) s 5
			| c == '"' || c == '/' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t'
				= count_more_escape_chars (i + 1) s 1
				= count_escape_chars (i + 1) s
		= 0
where
	count_more_escape_chars :: !Int !String !Int -> Int
	count_more_escape_chars i s n
		| i < size s
			#! c = s.[i]
			| c >= '0'
				| c <> '\\'
					= count_more_escape_chars (i + 1) s n
					= count_more_escape_chars (i + 1) s (n+1)
				| isControl c = count_more_escape_chars (i+1) s (n+5)
				| c == '"' || c == '/' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t'
					= count_more_escape_chars (i + 1) s (n+1)
					= count_more_escape_chars (i + 1) s n
			= n

//Copy structure to a string
copyNode :: !Int !JSONNode !*{#Char} -> *(!Int, !*{#Char})
copyNode start (JSONNull) buffer		= (start + 4, copyChars start 4 "null" buffer)
copyNode start (JSONBool True) buffer	= (start + 4, copyChars start 4 "true" buffer)
copyNode start (JSONBool False) buffer	= (start + 5, copyChars start 5 "false" buffer)
copyNode start (JSONInt x) buffer
  #! s = toString x
  = (start + size s, copyChars start (size s) s buffer)
copyNode start (JSONReal x) buffer
  #! s = toString x
  = (start + size s, copyChars start (size s) s buffer)
copyNode start (JSONString s) buffer
	#! reps = findChars 0 s
	| reps=:[!!]
		#! len = size s
		= (start + len + 2, copyChars (start + 1) len s {buffer & [start] = '"', [start + len + 1] = '"'})
		#! buffer & [start] = '"'
		#! (start,buffer) = copyAndReplaceChars 0 (start+1) reps s buffer
		= (start+1, {buffer & [start] = '"'})
	where
		// Copy the escaped string from the original and the replacements		
		copyAndReplaceChars :: !Int !Int ![!(Int,String)!] !String !*String -> (!Int,!*String)
		copyAndReplaceChars is id [!(ir,c):rs!] src dest
			#! (is,id,src,dest) = copyCharsI is id ir src dest
			#! dest = {dest & [id] = '\\', [id + 1] = c.[0]}
			#! dest = if (size c == 1) dest
				{dest & [id+2]=c.[1], [id+3]=c.[2], [id+4]=c.[3], [id+5]=c.[4]}
			= copyAndReplaceChars (is + 1) (id + size c + 1) rs src dest
		copyAndReplaceChars is id [!!] src dest
			= copyRemainingChars is id src dest

		copyRemainingChars :: !Int !Int !String !*String -> (!Int,!*String)
		copyRemainingChars is id src dest
			| is < size src
				= copyRemainingChars (is + 1) (id + 1) src {dest & [id] = src.[is]}
				= (id,dest)
copyNode start (JSONArray items) buffer
	#! (start,buffer)	= (start + 1, {buffer & [start] = '['})
	#! (start,buffer)	= copyArrayItems start items buffer
	= (start + 1, {buffer & [start] = ']'})
where
    copyArrayItems :: !Int ![JSONNode] !*String -> *(!Int, !*String)
	copyArrayItems start [] buffer = (start,buffer)
	copyArrayItems start [x] buffer = copyNode start x buffer
	copyArrayItems start [x:xs] buffer
		#! (start,buffer) = copyNode start x buffer
		= copyArrayItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONObject items) buffer
	#! (start, buffer) = (start + 1, {buffer & [start] = '{'})
	#! (start, buffer) = copyObjectItems start items buffer
	= (start + 1, {buffer &	[start] = '}'})
where
    copyObjectItems :: !Int ![(!String, !JSONNode)] !*String -> *(!Int, !*String)
	copyObjectItems start [] buffer = (start,buffer)
	copyObjectItems start [(l,x)] buffer
		# (start,buffer) = let len = size l in (start + len + 3 , copyChars (start + 1) len l {buffer & [start] = '"', [start + len + 1] = '"', [start + len + 2] = ':'})
		= copyNode start x buffer
	copyObjectItems start [(l,x):xs] buffer
		# (start,buffer) = let len = size l in (start + len + 3 , copyChars (start + 1) len l {buffer & [start] = '"', [start + len + 1] = '"', [start + len + 2] = ':'})
		# (start,buffer) = copyNode start x buffer
		= copyObjectItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONRaw x) buffer	= (start + size x, copyChars start (size x) x buffer) 	
copyNode start _ buffer				= (start,buffer)

copyChars :: !Int !Int !String !*String -> *String
copyChars offset i src dst
	| i>3
		#! di = offset + i
		#! dst & [di-4] = src.[i-4]
		#! dst & [di-3] = src.[i-3]
		#! dst & [di-2] = src.[i-2]
		#! dst & [di-1] = src.[i-1]
		= copyChars offset (i-4) src dst
	| i>1
		#! dst & [offset] = src.[0]
		#! dst & [offset+1] = src.[1]
		| i==3
			= {dst & [offset+2] = src.[2]}
			= dst
		| i==1
			= {dst & [offset] = src.[0]}
			= dst

//Basic JSON deserialization (just structure)
instance fromString JSONNode
where
	fromString s = fst (parse 0 s)

IsDigit c :== c >= '0' && c <= '9'

parse :: !Int !String -> (!JSONNode,!Int)
parse offset input
	| offset<size input
		#! c = input.[offset]
		| c=='"'
			#! offset=offset+1
			= parse_string offset offset input
		| c=='n' && offset+3<size input && input.[offset+1]=='u' && input.[offset+2]=='l' && input.[offset+3]=='l'
			= (JSONNull,offset+4)
		| c=='t' && offset+3<size input && input.[offset+1]=='r' && input.[offset+2]=='u' && input.[offset+3]=='e'
			= (JSONBool True, offset+4)
		| c=='f' && offset+4<size input && input.[offset+1]=='a' && input.[offset+2]=='l' && input.[offset+3]=='s' && input.[offset+4]=='e'
			= (JSONBool False, offset+5)
		| IsDigit c
			= parse_number (offset+1) offset input
		| c=='-' && offset+1<size input && IsDigit input.[offset+1]
			= parse_number (offset+2) offset input
		| c=='['
			= parse_array (offset+1) input
		| c=='{'
		 	= parse_object (offset+1) input
		| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
			= parse (skip_spaces (offset+1) input) input
			= (JSONError, offset)
		= (JSONError, offset)
where
	parse_string :: !Int !Int !{#Char} -> (!JSONNode,!Int)
	parse_string offset stringCharsOffset input
		| offset<size input
			#! c=input.[offset]
			| c <> '"'
				| c <> '\\'
					= parse_string (offset + 1) stringCharsOffset input
					= parse_string_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
				#! string = input % (stringCharsOffset,offset-1)
				= (JSONString string, offset+1)
			= (JSONError,offset) // missing '"'
	where
		parse_string_with_escape :: !Int !Int !{#Char} -> (!JSONNode,!Int)
		parse_string_with_escape offset stringCharsOffset input
			| offset<size input
				#! c = input.[offset]
				| c <> '"'
					| c <> '\\'
						= parse_string_with_escape (offset + 1) stringCharsOffset input
						= parse_string_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
					#! string = input % (stringCharsOffset,offset-1)
					= (JSONString (jsonUnescape string), offset+1)
				= (JSONError,offset) // missing '"'

	skip_spaces :: !Int !String -> Int
	skip_spaces offset input
		| offset<size input
			#! c = input.[offset]
			| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
				= skip_spaces (offset+1) input
				= offset
			= offset

	parse_number :: !Int !Int !{#Char} -> (!JSONNode,!Int)
	parse_number offset numberOffset input
		| offset>=size input
			#! i = toInt (input % (numberOffset,offset-1))
			= (JSONInt i,offset)
		#! c = input.[offset]
		| IsDigit c
			= parse_number (offset+1) numberOffset input
		| c<>'.'
			#! i = toInt (input % (numberOffset,offset-1))
			= (JSONInt i, offset)
			= parse_real (offset+1) numberOffset input
	where
		parse_real :: !Int !Int !{#Char} -> (!JSONNode,!Int)
		parse_real offset numberOffset input
			| offset>=size input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r,offset)
			#! c = input.[offset]
			| IsDigit c
				= parse_real (offset+1) numberOffset input
			| c<>'e' && c<>'E'
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r, offset)
			| offset+1<size input && IsDigit input.[offset+1]
				= parse_real_with_exponent (offset+2) numberOffset input
			| offset+2<size input && input.[offset+1]=='-' && IsDigit input.[offset+2]
				= parse_real_with_exponent (offset+3) numberOffset input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r, offset)
	
		parse_real_with_exponent :: !Int !Int !{#Char} -> (!JSONNode,!Int)
		parse_real_with_exponent offset numberOffset input
			| offset>=size input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r,offset)
			| IsDigit input.[offset]
				= parse_real_with_exponent (offset+1) numberOffset input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r, offset)

	parse_array :: !Int !{#Char} -> (!JSONNode,!Int)
	parse_array offset input
		| offset<size input && input.[offset]==']'
			= (JSONArray [], offset+1)
		#! offset = skip_spaces offset input
		| offset<size input && input.[offset]==']'
			= (JSONArray [], offset+1)
			= parse_array_items offset [] offset input
	where
		parse_array_items :: !Int !*[JSONNode] !Int !{#Char} -> (!JSONNode,!Int)
		parse_array_items offset items offset_after_bracket_open input
			#! (item,offset) = parse offset input
			| offset<size input && input.[offset]==','
				= parse_array_items (offset+1) [item:items] offset_after_bracket_open input
			| offset<size input && input.[offset]==']'
				= (JSONArray (reverse_append items [item]), offset+1)
			#! offset = skip_spaces offset input
			| offset<size input && input.[offset]==','
				= parse_array_items (offset+1) [item:items] offset_after_bracket_open input
			| offset<size input && input.[offset]==']'
				= (JSONArray (reverse_append items [item]), offset+1)
				= (JSONError, offset_after_bracket_open)

	parse_object :: !Int !{#Char} -> (!JSONNode,!Int)
	parse_object offset input
		| offset<size input && input.[offset]=='}'
			= (JSONObject [], offset+1)
		#! offset = skip_spaces offset input
		| offset<size input && input.[offset]=='}'
			= (JSONObject [], offset+1)
			= parse_object_items offset [] offset input
	where
		parse_object_items :: !Int !*[(!{#Char}, !JSONNode)] !Int !{#Char} -> (!JSONNode,!Int)
		parse_object_items offset items offset_after_bracket_open input
			| offset<size input
				| input.[offset]=='"'
					#! offset=offset+1
					#! (label,offset) = lex_label offset offset input
					| offset>=0
						| offset<size input && input.[offset]==':'
							= parse_object_items_after_label_and_colon label (offset+1) items offset_after_bracket_open input
							#! offset = skip_spaces offset input
							| offset<size input && input.[offset]==':'
								= parse_object_items_after_label_and_colon label (offset+1) items offset_after_bracket_open input
								= (JSONError, offset_after_bracket_open)
						= (JSONError, offset_after_bracket_open)
					#! c = input.[offset]
					| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
						= parse_object_items (skip_spaces (offset+1) input) items offset_after_bracket_open input
						= (JSONError, offset_after_bracket_open)
				= (JSONError, offset_after_bracket_open)
		where
			lex_label :: !Int !Int !{#Char} -> (!{#Char},!Int)
			lex_label offset stringCharsOffset input
				| offset<size input
					#! c=input.[offset]
					| c <> '"'
						| c <> '\\'
							= lex_label (offset + 1) stringCharsOffset input
							= lex_label_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
						#! string = input % (stringCharsOffset,offset-1)
						= (string, offset+1)
					= ("",-1) // missing '"'

			lex_label_with_escape :: !Int !Int !{#Char} -> (!{#Char},!Int)
			lex_label_with_escape offset stringCharsOffset input
				| offset<size input
					#! c=input.[offset]
					| c <> '"'
						| c <> '\\'
							= lex_label_with_escape (offset + 1) stringCharsOffset input
							= lex_label_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
						#! string = input % (stringCharsOffset,offset-1)
						= (jsonUnescape string, offset+1)
					= ("",-1) // missing '"'

		parse_object_items_after_label_and_colon :: !{#Char} !Int !*[(!{#Char}, !JSONNode)] !Int !{#Char} -> (!JSONNode,!Int)
		parse_object_items_after_label_and_colon label offset items offset_after_brace_open input
			#! (item,offset) = parse offset input
			| offset<size input && input.[offset]==','
				= parse_object_items (offset+1) [(label,item):items] offset_after_brace_open input
			| offset<size input && input.[offset]=='}'
				= (JSONObject (reverse_append items [(label,item)]), offset+1)
			#! offset = skip_spaces offset input
			| offset<size input && input.[offset]==','
				= parse_object_items (offset+1) [(label,item):items] offset_after_brace_open input
			| offset<size input && input.[offset]=='}'
				= (JSONObject (reverse_append items [(label,item)]), offset+1)
				= (JSONError, offset_after_brace_open)

	reverse_append :: !*[.a] !*[.a] -> *[.a]
	reverse_append [hd:tl] list	= reverse_append tl [hd:list]
	reverse_append [] list		= list

instance <<< JSONNode
where
	(<<<) f JSONNull            = f <<< "null"
	(<<<) f (JSONBool True)     = f <<< "true"
	(<<<) f (JSONBool False)    = f <<< "false"
	(<<<) f (JSONInt i)         = f <<< i
	(<<<) f (JSONReal r)        = f <<< r
	(<<<) f (JSONString s)      = f <<< '"' <<< jsonEscape s <<< '"'
	(<<<) f (JSONArray nodes)   = printNodes nodes (f <<< "[") <<< "]"
	where
		printNodes :: [JSONNode] *File -> *File
		printNodes []         f = f
		printNodes [n]        f = f <<< n
		printNodes [n:ns]     f = printNodes ns (f <<< n <<< ",")
	(<<<) f (JSONObject nodes)  = printNodes nodes (f <<< "{") <<< "}"
	where
		printNodes :: [(String,JSONNode)] *File -> *File
		printNodes []         f = f
		printNodes [(k,v)]    f = f <<< '"' <<< jsonEscape k <<< "\":" <<< v
		printNodes [(k,v):ns] f = printNodes ns (f <<< '"' <<< jsonEscape k <<< "\":" <<< v <<< ",")
	(<<<) f (JSONRaw s)         = f <<< s

//Escape a string
jsonEscape :: !String -> String
jsonEscape src
	#! reps = findChars 0 src
	= case reps of
		[!!] -> src
		reps -> copyAndReplaceChars 0 0 reps src (createArray (size src + destSize reps) '\0')
where
	destSize [!!] = 0
	destSize [!(_,x):xs!] = size x - 1 + destSize xs

	//Build the escaped string from the original and the replacements		
	copyAndReplaceChars :: !Int !Int ![!(!Int, !String)!] !String !*String -> *String
	copyAndReplaceChars is id reps=:[!(ir,c):rs!] src dest
		#! (is,id,src,dest) = copyCharsI is id ir src dest
		#! dest = {dest & [id] = '\\', [id + 1] = c.[0]}
		#! dest = if (size c == 1) dest
			{dest & [id+2]=c.[1], [id+3]=c.[2], [id+4]=c.[3], [id+5]=c.[4]}
		= copyAndReplaceChars (is + 1) (id + size c + 1) rs src dest
	copyAndReplaceChars is id [!!] src dest
		= copyRemainingChars is id src dest

//Find the special characters
findChars :: Int String -> [!(Int,String)!]
findChars i s
	| i < size s
		#! c = s.[i]
		| c >= '0'
			| c <> '\\'
				= findChars (i + 1) s
				= [!(i,toString c): findChars (i + 1) s!]
			| c == '"' || c == '/'
				= [!(i,toString c): findChars (i + 1) s!]
			| c == '\b'
				= [!(i,"b"): findChars (i + 1) s!]
			| c == '\f'
				= [!(i,"f"): findChars (i + 1) s!]
			| c == '\n'
				= [!(i,"n"): findChars (i + 1) s!]
			| c == '\r'
				= [!(i,"r"): findChars (i + 1) s!]
			| c == '\t'
				= [!(i,"t"): findChars (i + 1) s!]
			| isControl c
				= [!(i,"u" +++ toHex (toInt c)): findChars (i + 1) s!]
				= findChars (i + 1) s
		= [!!]
	where
		toHex :: !Int -> !String
		toHex c = {#'0', '0', toHexDigit (c / 16), toHexDigit (c rem 16)}
		toHexDigit c
		| c > 15 = toHexDigit (c rem 16)
		| c < 0 = toHexDigit (~c)
		| c < 10 = toChar (c + 48)
		| otherwise = toChar (c + 87)

//Unescape a string
jsonUnescape :: !String -> String
jsonUnescape src
	#! reps = findChars 0 src
	= case reps of
		[!!] -> src
		reps -> copyAndReplaceChars 0 0 reps src (createArray (size src - destSize reps) '\0')
where
	destSize :: [!(!Int, !Int, !Char)!] -> Int
	destSize [!!] = 0
	destSize [!(_,x,_):xs!] = x-1 + destSize xs

	//Find the special characters
	findChars :: !Int !String -> [!(!Int, !Int, !Char)!]
	findChars i s
		| i+1>=size s
			= [!!]
		#! c0 = s.[i]
		| c0 == '\\'
			#! c1 = s.[i+1]
			| c1 == 'u'
				#! rc = toChar (parseHex s (i+2))
				= [!(i,6,rc):findChars (i+6) s!]
			#! rc = rep c1
			= [!(i,2,rc): findChars (i + 2) s!]
			= findChars (i + 1) s
	where
			parseHex :: !String !Int -> Int
			parseHex s i = ph s.[i] * 16^3 + ph s.[i+1] * 16^2 + 
					ph s.[i+2] * 16 + ph s.[i+3] 
				where 
					ph c
					| isDigit c = digitToInt c
					| c <= 'f' && c >= 'a' = toInt c - 87
					| c <= 'F' && c >= 'A' = toInt c - 55
					= 0

            rep :: !Char -> Char
			rep '\\'	= '\\'
			rep '"'		= '"'
			rep '/'		= '/'
			rep 'b'		= '\b'
			rep 'f'		= '\f'
			rep 'n'		= '\n'
			rep 'r'		= '\r'
			rep 't'		= '\t'
			rep c		= c

	//Build the escaped string from the original and the replacements		
	copyAndReplaceChars :: !Int !Int ![!(!Int, !Int, !Char)!] !String !*String -> *String
	copyAndReplaceChars is id reps=:[!(ir,il,c):rs!] src dest
		//Copy until the replace
		#! (is,id,src,dest) = copyCharsI is id ir src dest
		=	copyAndReplaceChars (is + il) (id + 1) rs src {dest & [id] = c}
	copyAndReplaceChars is id [!!] src dest
		= copyRemainingChars is id src dest

copyCharsI :: !Int !Int !Int !String !*String -> (!Int,!Int,!String,!*String)
copyCharsI is id iend src dest
	| is < iend		= copyCharsI (is + 1) (id + 1) iend src {dest & [id] = src.[is]}
					= (is,id,src,dest)

copyRemainingChars :: !Int !Int !String !*String -> *String
copyRemainingChars is id src dest
	| is < size src	= copyRemainingChars (is + 1) (id + 1) src {dest & [id] = src.[is]}
					= dest

//-------------------------------------------------------------------------------------------

toJSON :: !a -> JSONNode | JSONEncode{|*|} a
toJSON x = toJSON` False x

toJSONInField :: !a -> JSONNode | JSONEncode{|*|} a
toJSONInField x = toJSON` True x

toJSON` :: !Bool !a -> JSONNode | JSONEncode{|*|} a
toJSON` flag x = case (JSONEncode{|*|} flag x) of
	[node]	= node
	_		= JSONError 

/*
* Generic JSON encoder
*/
generic JSONEncode t :: !Bool !t -> [JSONNode]

JSONEncode{|Int|} _ x = [JSONInt x]
JSONEncode{|Real|} _ x = [JSONReal x]
JSONEncode{|Char|} _ x = [JSONString {x}]
JSONEncode{|Bool|} _ x = [JSONBool x]
JSONEncode{|String|} _ x = [JSONString x]
JSONEncode{|UNIT|} _ (UNIT) = []
JSONEncode{|PAIR|} fx fy _ (PAIR x y) = fx False x ++ fy False y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list
JSONEncode{|EITHER|} fx fy _ (LEFT x) = fx False x
JSONEncode{|EITHER|} fx fy _ (RIGHT y) = fy False y
JSONEncode{|OBJECT|} fx _ (OBJECT x) = fx False x
JSONEncode{|CONS of {gcd_name}|} fx _ (CONS x)
  = [JSONArray [JSONString gcd_name : fx False x]]
JSONEncode{|RECORD of {grd_fields}|} fx _ (RECORD x)
  = [JSONObject [(name, o) \\ o <- fx False x & name <- grd_fields | isNotNull o]]
  where
  isNotNull :: !JSONNode -> Bool
  isNotNull JSONNull = False
  isNotNull _ = True
JSONEncode{|FIELD|} fx _ (FIELD x) = fx True x
JSONEncode{|[]|} fx _ x = [JSONArray (flatten [fx False e \\ e <- x])]
JSONEncode{|(,)|} fx fy _ (x,y) = [JSONArray (fx False x ++ fy False y)]
JSONEncode{|(,,)|} fx fy fz _ (x,y,z) = [JSONArray (fx False x ++ fy False y ++ fz False z)]
JSONEncode{|(,,,)|} fx fy fz fi _ (x,y,z,i) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i)]
JSONEncode{|(,,,,)|} fx fy fz fi fj _ (x,y,z,i,j) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j)]
JSONEncode{|(,,,,,)|} fx fy fz fi fj fk _ (x,y,z,i,j,k) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k)]
JSONEncode{|(,,,,,,)|} fx fy fz fi fj fk fl _ (x,y,z,i,j,k,l) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k ++ fl False l)]
JSONEncode{|(,,,,,,,)|} fx fy fz fi fj fk fl fm _ (x,y,z,i,j,k,l,m) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k ++ fl False l ++ fm False m)]
JSONEncode{|{}|} fx _ x = [JSONArray (flatten [fx False e \\ e <-: x])]
JSONEncode{|{!}|} fx _ x = [JSONArray (flatten [fx False e \\ e <-: x])]
JSONEncode{|Maybe|} fx inField (Just x) = if inField (fx False x) [JSONArray [JSONString "Just" : fx False x]]
JSONEncode{|Maybe|} fx inField Nothing = if inField [JSONNull] [JSONArray [JSONString "Nothing"]]
JSONEncode{|JSONNode|} _ node = [node]

//-------------------------------------------------------------------------------------------
fromJSON :: !JSONNode -> Maybe a | JSONDecode{|*|} a
fromJSON node = fst (JSONDecode{|*|} False [node])

/*
* Generic JSON parser, using a list of tokens
*/
generic JSONDecode t :: !Bool ![JSONNode] -> (!Maybe t, ![JSONNode])

JSONDecode{|Int|} _ [JSONInt i:xs]		= (Just i, xs)
JSONDecode{|Int|} _ l					= (Nothing, l)

JSONDecode{|Real|} _ [JSONReal r:xs]	= (Just r, xs)
JSONDecode{|Real|} _ [JSONInt i:xs]		= (Just (toReal i), xs)
JSONDecode{|Real|} _ l					= (Nothing, l)

JSONDecode{|Char|} _ l=:[JSONString s:xs]
	| size s == 1						= (Just s.[0],xs)
										= (Nothing, l)
JSONDecode{|Char|} _ l					= (Nothing, l)

JSONDecode{|Bool|} _ [JSONBool b:xs]	= (Just b,xs)
JSONDecode{|Bool|} _ l					= (Nothing, l)

JSONDecode{|String|} _ [JSONString s:xs]= (Just s, xs)
JSONDecode{|String|} _ l				= (Nothing, l)

JSONDecode{|UNIT|} _ l					= (Just UNIT, l)

JSONDecode{|PAIR|} fx fy _ l = d1 fy (fx False l) l
  where
  d1 :: !(Bool [JSONNode] -> (!Maybe b, ![JSONNode])) !(!Maybe a, ![JSONNode]) ![JSONNode]
     -> (!Maybe (PAIR a b), ![JSONNode])
  d1 fy (Just x,xs)  l = d2 x (fy False xs) l
  d1 _  (Nothing, _) l = (Nothing, l)

  d2 :: !a !(!Maybe b, ![JSONNode]) ![JSONNode] -> (!Maybe (PAIR a b), ![JSONNode])
  d2 x (Just y, ys) l = (Just (PAIR x y), ys)
  d2 x (Nothing, _) l = (Nothing, l)

JSONDecode{|EITHER|} fx fy _ l = case fx False l of
	(Just x, xs)				= (Just (LEFT x),xs)
	(Nothing, xs)				= case fy False l of
		(Just y, ys)			= (Just (RIGHT y),ys)
		(Nothing, ys)			= (Nothing, l)

JSONDecode{|OBJECT|} fx _ l = case fx False l of
	(Just x, xs)	= (Just (OBJECT x),xs)
	_				= (Nothing, l)

JSONDecode{|CONS of {gcd_name}|} fx _ l=:[JSONArray [JSONString name:fields] :xs]
	| name == gcd_name				= case fx False fields of
		(Just x, _)					= (Just (CONS x), xs)
		_							= (Nothing, l)
	| otherwise						= (Nothing, l)		
JSONDecode{|CONS|} fx _ l = (Nothing, l)

JSONDecode{|RECORD|} fx _ l=:[obj=:JSONObject fields : xs] = d (fx False [obj]) xs l
  where
  d :: !(!Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSONDecode{|RECORD|} fx _ l=:[obj=:JSONArray fields : xs] = d (fx False [obj]) xs l
  where
  d :: !(!Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSONDecode{|RECORD|} fx _ l = (Nothing,l)

JSONDecode{|FIELD of {gfd_name}|} fx _ l =:[JSONObject fields]
  #! field = findField gfd_name fields
  = case fx True field of
      (Just x, _) = (Just (FIELD x), l)
      (_, _)      = (Nothing, l)
  where
  findField :: !String ![(!String, !JSONNode)] -> [JSONNode]
  findField match [(l,x):xs]
    | l == match = [x]
    | otherwise  = findField match xs
  findField match [] = []
JSONDecode{|FIELD of {gfd_index}|} fx _ l =:[JSONArray fields]
  #! field = fields !! gfd_index
  = case fx True [field] of
      (Just x, _) = (Just (FIELD x), l)
      (_, _)      = (Nothing, l)
JSONDecode{|FIELD|} fx _ l = (Nothing, l)

JSONDecode{|[]|} fx _ l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just x, xs)
		_				= (Nothing, l)
JSONDecode{|[]|} fx _ l = (Nothing, l)

JSONDecode{|(,)|} fx fy _ l =:[JSONArray [xo,yo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)		= (Just (x,y), xs)
			_				= (Nothing, l)
		_					= (Nothing, l)
JSONDecode{|(,)|} fx fy _ l	= (Nothing, l)

JSONDecode{|(,,)|} fx fy fz _ l =:[JSONArray [xo,yo,zo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)			= case fz False [zo] of
				(Just z,_)		= (Just (x,y,z), xs)
				_				= (Nothing, l)
			_					= (Nothing, l)
		_						= (Nothing, l)
JSONDecode{|(,,)|} fx fy fz _ l	= (Nothing, l)

JSONDecode{|(,,,)|} fx fy fz fi _ l =:[JSONArray [xo,yo,zo,io]:xs]
	= case fx False [xo] of
		(Just x,_) = case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_)		= (Just (x,y,z,i), xs)
					_				= (Nothing, l)
				_					= (Nothing, l)
			_						= (Nothing, l)
		_							= (Nothing, l)
JSONDecode{|(,,,)|} fx fy fz fi _ l	= (Nothing, l)

JSONDecode{|(,,,,)|} fx fy fz fi fj _ l =:[JSONArray [xo,yo,zo,io,jo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_)	= case fj False [jo] of
						(Just j,_)		= (Just (x,y,z,i,j), xs)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,)|} fx fy fz fi fj _ l	= (Nothing, l)

JSONDecode{|(,,,,,)|} fx fy fz fi fj fk _ l =:[JSONArray [xo,yo,zo,io,jo,ko]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_)	= case fj False [jo] of
						(Just j,_)		= case fk False [ko] of
                            (Just k, _) = (Just (x,y,z,i,j,k), xs)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,,)|} fx fy fz fi fj fk _ l	= (Nothing, l)

JSONDecode{|(,,,,,,)|} fx fy fz fi fj fk fm _ l =:[JSONArray [xo,yo,zo,io,jo,ko,mo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_) = case fj False [jo] of
						(Just j,_) = case fk False [ko] of
                            (Just k, _) = case fm False [mo] of
                              (Just m, _) = (Just (x,y,z,i,j,k,m), xs)
                              _           = (Nothing, l)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,,,)|} fx fy fz fi fj fk fm _ l	= (Nothing, l)

JSONDecode{|(,,,,,,,)|} fx fy fz fi fj fk fm fn _ l =:[JSONArray [xo,yo,zo,io,jo,ko,mo,no]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_) = case fj False [jo] of
						(Just j,_) = case fk False [ko] of
                            (Just k, _) = case fm False [mo] of
                              (Just m, _) = case fn False [no] of
                                (Just n, _) = (Just (x,y,z,i,j,k,m,n), xs)
                                _           = (Nothing, l)
                              _           = (Nothing, l)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,,,,)|} fx fy fz fi fj fk fm fn _ l	= (Nothing, l)

JSONDecode{|{}|} fx _ l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{}|} fx _ l = (Nothing, l)

JSONDecode{|{!}|} fx _ l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{!}|} fx _ l = (Nothing, l)

decodeItems :: !(Bool [JSONNode] -> (!Maybe a, ![JSONNode])) ![JSONNode] -> Maybe [a]
decodeItems fx [] 		= Just []
decodeItems fx [ox:oxs]	= case fx False [ox] of
	(Just x, _)	= case decodeItems fx oxs of
		(Just xs)	= Just [x:xs]
		_ 			= Nothing
	_			= Nothing

// When not in a record, treat Maybe normally
JSONDecode{|Maybe|} fx False [JSONArray [JSONString "Nothing"]:xs] = (Just Nothing, xs)
JSONDecode{|Maybe|} fx False [JSONArray [JSONString "Just":l]:xs]
  = case fx False l of
      (Just x, _) = (Just (Just x), xs)
      _           = (Nothing, l)
// Maybe is treated a bit special in record fields for efficiency
JSONDecode{|Maybe|} fx True [JSONNull:xs] = (Just Nothing, xs) // Interpret null as Nothing
JSONDecode{|Maybe|} fx True []            = (Just Nothing, []) // Interpret absentness as Nothing
JSONDecode{|Maybe|} fx True l
  = case fx False l of                  // Interpret existense as Just
      (Just x,xs)                         = (Just (Just x), xs)
      _                                   = (Nothing, l)
JSONDecode{|Maybe|} _ _ l               = (Nothing, l) // If all else fails... Nothing

JSONDecode{|JSONNode|} _ [node:xs]      = (Just node, xs)
JSONDecode{|JSONNode|} True []			= (Just JSONNull, []) //In record fields, fields with value JSONNull are removed
JSONDecode{|JSONNode|} _ l				= (Nothing, l)

jsonQuery :: !String !JSONNode -> Maybe a | JSONDecode{|*|} a
jsonQuery path node
	= case (findNode (split "/" path) node ) of
		Just child	= fromJSON child
		Nothing		= Nothing
where
	findNode :: ![String] !JSONNode -> Maybe JSONNode
	findNode [] node	= Just node
	findNode [s:ss] (JSONObject fields)
		= case findField s fields of
			Just f	= findNode ss f
			Nothing	= Nothing
	findNode [s:ss] (JSONArray items)
		#! index = toInt s
		| index >= 0 && index < length items	= findNode ss (items !! index)
		| otherwise								= Nothing
	findNode _ _		= Nothing
	
    findField :: !String ![(!String, !JSONNode)] -> Maybe JSONNode
	findField s []			= Nothing
	findField s [(l,x):xs]	= if (l == s) (Just x) (findField s xs)

instance == JSONNode where
  (==) JSONNull        JSONNull        = True
  (==) (JSONBool x)    (JSONBool y)    = x == y
  (==) (JSONInt x)     (JSONInt y)     = x == y
  (==) (JSONReal x)    (JSONReal y)    = toString x == toString y
  (==) (JSONInt x)     (JSONReal y)    = toString (toReal x) == toString y
  (==) (JSONReal x)    (JSONInt y)     = toString x == toString (toReal y)
  (==) (JSONString x)  (JSONString y)  = x == y
  (==) (JSONArray xs)  (JSONArray ys)  = xs == ys
  (==) (JSONObject xs) (JSONObject ys) = sortBy cmpFst (filter (notNull o snd) xs) == sortBy cmpFst (filter (notNull o snd) ys)
    where
    cmpFst :: !(!a, b) !(!a, c) -> Bool | < a
    cmpFst a b = fst a < fst b
    notNull :: !JSONNode -> Bool
    notNull JSONNull = False
    notNull _        = True
  (==) (JSONRaw x)     (JSONRaw y)     = x == y
  (==) JSONError       JSONError       = True
  (==) _               _               = False

jsonPrettyPrint :: !JSONNode -> String
jsonPrettyPrint json = display (renderPretty 0.0 400 (pretty json))

instance Pretty JSONNode
where
	pretty JSONNull 			= string "null"
	pretty (JSONBool x)			= string (if x "true" "false")
	pretty (JSONInt x)			= string (toString x)
	pretty (JSONReal x)			= string (toString x)
	pretty (JSONString x)		= dquotes (string (jsonEscape x))
	pretty (JSONArray nodes)	= list (map pretty nodes)
	pretty (JSONObject attr)	= encloseSep lbrace rbrace comma [dquotes (string label) <-> colon <-> pretty val \\ (label,val) <- attr]
	pretty (JSONRaw x)			= string x
	pretty JSONError			= string "null"
