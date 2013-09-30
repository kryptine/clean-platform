definition module Unicode

import StdClass
from UChar import :: UChar

:: UString :== [UChar]

// String is supposed be ASCII
instance fromString UString

instance % UString
instance +++ UString

class fromUnicode a :: UString -> a
class toUnicode a :: a -> UString



