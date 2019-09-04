implementation module Text.GenXML.Gast

import StdEnv
import Gast
import Data.Func, Data.Functor, Data.Maybe, Data.Maybe.Gast, Data.List, Data.Bifunctor, Data.Tuple
import Text.GenXML
import Control.GenBimap

derive genShow XMLDoc, XMLQName, XMLNode, XMLAttr
derive ggen                      XMLNode, XMLAttr

// TODO: Generate URIs for namespaces, instead of using names.
ggen{|XMLDoc|} st =
	[ XMLDoc
		(unNameString <$> defaultNamespace)
		(bifmap unNameString unNameString <$> namespaces)
		(XMLElem rootName rootAttrs rootChildren)
	\\ (defaultNamespace, namespaces, rootName, rootAttrs, rootChildren) <- ggen{|*|} st
	]

ggen{|XMLQName|} st = [XMLQName (unNameString <$> namespace) (unNameString name) \\ (namespace, name) <- ggen{|*|} st]

:: NameString =: NameString String

unNameString :: !NameString -> String
unNameString (NameString str) = str

// TODO: Also include capital letters and other valid characters.
ggen{|NameString|} _ = [NameString str \\ str <- ggenString 7 4.0 97 122 aStream | str <> ""]
