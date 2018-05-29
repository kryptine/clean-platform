implementation module Graphics.Scalable.Types

import Data.Maybe
import Data.List
import Data.GenEq
from Text.HTML import :: SVGColor (..)
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import Text.GenPrint
import StdBool, StdInt, StdReal, StdString
import Graphics.Scalable.Internal.Types

:: FontDef
  = { fontfamily`  :: !String
    , fontysize`   :: !Real
    , fontstretch` :: !String
    , fontstyle`   :: !String
    , fontvariant` :: !String
    , fontweight`  :: !String
    }

normalFontDef :: !String !Real -> FontDef // (normalFontDef family size) sets all other fields to "normal"
normalFontDef family size
  = { fontfamily`  = family
    , fontysize`   = to2dec (max zero size)  // make sure the size can be converted correctly to the client and vice versa
    , fontstretch` = "normal"
    , fontstyle`   = "normal"
    , fontvariant` = "normal"
    , fontweight`  = "normal"
    }

setfontfamily :: !String !FontDef -> FontDef
setfontfamily family fontdef = {FontDef | fontdef & fontfamily` = family}

setfontysize :: !Real !FontDef -> FontDef
setfontysize ysize fontdef = {FontDef | fontdef & fontysize` = to2dec ysize}

setfontstretch :: !String !FontDef -> FontDef
setfontstretch stretch fontdef = {FontDef | fontdef & fontstretch` = stretch}

setfontstyle :: !String !FontDef -> FontDef
setfontstyle style fontdef = {FontDef | fontdef & fontstyle` = style}

setfontvariant :: !String !FontDef -> FontDef
setfontvariant variant fontdef = {FontDef | fontdef & fontvariant` = variant}

setfontweight :: !String !FontDef -> FontDef
setfontweight weight fontdef = {FontDef | fontdef & fontweight` = weight}

getfontfamily :: !FontDef -> String
getfontfamily {FontDef | fontfamily`} = fontfamily`

getfontysize :: !FontDef -> Real
getfontysize {FontDef | fontysize`} = fontysize`

getfontstretch :: !FontDef -> String
getfontstretch {FontDef | fontstretch`} = fontstretch`

getfontstyle :: !FontDef -> String
getfontstyle {FontDef | fontstyle`} = fontstyle`

getfontvariant :: !FontDef -> String
getfontvariant {FontDef | fontvariant`} = fontvariant`
getfontweight :: !FontDef -> String
getfontweight {FontDef | fontweight`} = fontweight`

to2dec :: !Real -> Real
to2dec r = toReal (toInt (r * 100.0)) / 100.0

instance == FontDef    where  == fd1 fd2 = fd1 === fd2
instance <  FontDef    where  <  fd1 fd2 =  fd1.fontfamily`  < fd2.fontfamily`
                                         || fd1.fontysize`   < fd2.fontysize`
                                         || fd1.fontstretch` < fd2.fontstretch`
                                         || fd1.fontstyle`   < fd2.fontstyle`
                                         || fd1.fontvariant` < fd2.fontvariant`
                                         || fd1.fontweight`  < fd2.fontweight`
derive gEq        FontDef
derive JSEncode   FontDef
derive JSDecode   FontDef
derive JSONEncode FontDef
derive JSONDecode FontDef
derive gPrint     FontDef
instance toString FontDef where toString font = printToString font

derive gEq LineMarkerPos
instance == LineMarkerPos  where == lm1 lm2 = lm1 === lm2

instance toSVGColor String where toSVGColor name = SVGColorText name
instance toSVGColor RGB    where toSVGColor {RGB | r, g, b} = SVGRGB r g b

instance zero RGB where zero = { r = 0, g = 0, b = 0 }
