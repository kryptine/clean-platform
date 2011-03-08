definition module XML

/**
* This module provides data types for easy construction of XML documents.
*/

import StdOverloaded
from Maybe	import :: Maybe
from Error	import :: MaybeErrorString, :: MaybeError

:: XMLDoc = XMLDoc !(Maybe XMLURI) ![(!XMLNamespacePrefix,!XMLURI)] !XMLNode

:: XMLNode	= XMLElem !XMLQName ![XMLAttr] ![XMLNode]
			| XMLText !String

:: XMLAttr = XMLAttr !XMLQName !String

:: XMLQName = XMLQName !(Maybe XMLNamespacePrefix) !XMLName

:: XMLNamespacePrefix :== String
:: XMLURI :== String
:: XMLName :== String

/**
* Create an XMLQName containing an unqualified name from a String
* @param Unqualified name
* @return XMLQName containing the unqualified name
*/
uname ::         !String -> XMLQName

/**
* Create an XMLQName containing a qualified name from a String
* @param Qualified name
* @return XMLQName containing the qualified name
*/
qname :: !XMLNamespacePrefix !String -> XMLQName

instance toString XMLDoc
instance fromString (MaybeErrorString XMLDoc)

