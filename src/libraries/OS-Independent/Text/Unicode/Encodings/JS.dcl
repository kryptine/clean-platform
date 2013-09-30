definition module JS

import StdClass, Unicode

// encode Unicode String as a JS string literal

:: JSLit

instance fromUnicode JSLit 
instance toUnicode JSLit 

instance fromString JSLit 
instance toString JSLit 

toJSLiteral :: !UString -> String
