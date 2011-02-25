implementation module XML

import _SystemArray
import StdBool
import StdInt
import StdList
import Maybe

uname :: !String -> XMLQName
uname name = XMLQName Nothing name

qname :: !String !String -> XMLQName
qname namespace name = XMLQName (Just namespace) name

addNamespaces :: !String [(!String,!String)] !XMLNode -> XMLNode
addNamespaces defaultNamespace namespaces (XMLElem qname attrs children)
	# ns = [ XMLAttr (XMLQName Nothing "xmlns") defaultNamespace
           : map (\(prefix,uri) -> XMLAttr (XMLQName (Just "xmlns") prefix) uri) namespaces
           ]
	= (XMLElem qname (ns ++ attrs) children)
	
docSize :: !XMLDoc -> Int
docSize (XMLDoc defaultNamespace namespaces documentElement)
	# documentElement = addNamespaces defaultNamespace namespaces documentElement
	= 37 + nodeSize documentElement

nodeSize :: !XMLNode -> Int
nodeSize (XMLText text) = escapedSize text
nodeSize (XMLElem qname attrs children)
	# attrsSize = sum (map attrSize attrs) + length attrs
 	= if (isEmpty children) 
 		(3 + qnameSize qname + attrsSize)
		(5 + 2 * qnameSize qname + attrsSize + sum (map nodeSize children))

attrSize :: !XMLAttr -> Int
attrSize (XMLAttr qname value) = 3 + qnameSize qname + escapedSize value

qnameSize :: !XMLQName -> Int
qnameSize (XMLQName Nothing   name) = size name
qnameSize (XMLQName (Just ns) name) = 1 + size ns + size name

//Calculates the number of chars in a string when xml special characters are escaped
escapedSize :: !{#Char} -> Int
escapedSize s = escapedSize` s (size s) 0
where
	escapedSize` s n i
		| i == n = 0
		| s.[i] == '<' = 4 + escapedSize` s n (i + 1)
		| s.[i] == '>' = 4 + escapedSize` s n (i + 1)
		| s.[i] == '&' = 5 + escapedSize` s n (i + 1)
		| otherwise = 1 + escapedSize` s n (i + 1)

serializeDoc :: !XMLDoc !*{#Char} !Int -> (!*{#Char}, !Int)
serializeDoc (XMLDoc defaultNamespace namespaces documentElement) dest dest_i
	# documentElement = addNamespaces defaultNamespace namespaces documentElement
	# (dest,dest_i) = copyChars "<?xml version=\"1.0\" standalone=\"no\"?>" 0 False dest dest_i
	= serializeNode documentElement dest dest_i 

serializeNode :: !XMLNode !*{#Char} !Int -> (!*{#Char}, !Int)
serializeNode (XMLText text) dest dest_i = copyChars text 0 True dest dest_i
serializeNode (XMLElem qname attrs []) dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i
	# (dest,dest_i) = serializeMap serializeAttr attrs dest dest_i
	# dest = {dest & [dest_i] = '/'}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '>'}
	= (dest,dest_i + 1)
serializeNode (XMLElem qname attrs children) dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i
	# (dest,dest_i) = serializeMap serializeAttr attrs dest dest_i
	# dest = {dest & [dest_i] = '>'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeMap serializeNode children dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '/'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i
	# dest = {dest & [dest_i] = '>'}
	= (dest,dest_i + 1)

serializeMap f [] dest dest_i = (dest, dest_i)
serializeMap f [x:xs] dest dest_i
	# (dest, dest_i) = f x dest dest_i
	= serializeMap f xs dest dest_i

serializeAttr :: !XMLAttr !*{#Char} !Int -> (!*{#Char}, !Int)
serializeAttr (XMLAttr qname value) dest dest_i
	# dest = {dest & [dest_i] = ' '}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i 
	# dest = {dest & [dest_i] = '='}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '"'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = copyChars value 0 True dest dest_i
	# dest = {dest & [dest_i] = '"'}
	# dest_i = dest_i + 1
	= (dest,dest_i)
	
serializeQName :: !XMLQName !*{#Char} !Int -> (!*{#Char}, !Int)
serializeQName (XMLQName Nothing   name) dest dest_i = copyChars name 0 False dest dest_i
serializeQName (XMLQName (Just ns) name) dest dest_i
	# (dest, dest_i) = copyChars ns 0 False dest dest_i
	# dest = {dest & [dest_i] = ':'}
	# dest_i = dest_i + 1
	= copyChars name 0 False dest dest_i

copyChars :: !{#Char} !Int !Bool !*{#Char} !Int -> (!*{#Char},!Int)
copyChars src src_i escape dest dest_i
	| src_i == (size src) = (dest, dest_i)
	| otherwise	
		| escape && (src.[src_i] == '<')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'l', [dest_i + 2] = 't', [dest_i + 3] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 4)
		| escape && (src.[src_i] == '>')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'g', [dest_i + 2] = 't', [dest_i + 3] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 4)
		| escape && (src.[src_i] == '&')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'a', [dest_i + 2] = 'm', [dest_i + 3] = 'p', [dest_i + 4] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 5)
		| otherwise
			# dest = {dest & [dest_i] = src.[src_i]}
			= copyChars src (src_i + 1) escape dest (dest_i + 1)

instance toString XMLDoc
where
	toString doc
		# docsize = docSize doc
		# docstring = createArray docsize '\0'
		# (docstring,_) = serializeDoc doc docstring 0
		= docstring
