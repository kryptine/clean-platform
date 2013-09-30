definition module Text.Unicode.Encodings.JS

import StdClass, Text.Unicode

// encode Unicode String as a JS/SAPL (SAPL also uses this encoding) String literal.

:: JSLit

instance fromUnicode JSLit 
instance toUnicode JSLit 

instance fromString JSLit 
instance toString JSLit 

