definition module Data.Unicode.UString

import StdClass
from Data.Unicode.UChar import :: UChar

:: UString :== [UChar]

instance fromString UString

instance % UString
instance +++ UString
