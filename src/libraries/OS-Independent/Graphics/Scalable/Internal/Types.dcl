definition module Graphics.Scalable.Internal.Types

import iTasks.UI.JavaScript
import Graphics.Scalable.Types
import iTasks.Internal.Generic.Visualization
import Text.GenPrint
from   Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: Span
  = PxSpan      !MilliInt                  // (PxSpan a) is a milli-pixels
  | LookupSpan  !LookupSpan                // (LookupSpan a) needs to be looked up after computing dimensions
  | AddSpan     !Span !Span                // (AddSpan a b) is span a + span b
  | SubSpan     !Span !Span                // (SubSpan a b) is span a - span b
  | MulSpan     !Span !Span                // (MulSpan a b) is span a * span k
  | DivSpan     !Span !Span                // (DivSpan a b) is span a / span k
  | AbsSpan     !Span                      // (AbsSpan a)  is absolute value of span a
  | MinSpan     ![Span]                    // (MinSpan as) is minimum span value in as
  | MaxSpan     ![Span]                    // (MaxSpan as) is maximum span value in as
:: LookupSpan
  = ColumnXSpan !ImageTag !Int             // (ColumnXSpan t a) is x-span of column number a in grid tagged with t
  | RowYSpan    !ImageTag !Int             // (RowYSpan t a) is y-span of row number a in grid tagged with t
  | ImageXSpan  !ImageTag                  // (ImageXSpan t) is x-span of image tagged with t
  | ImageYSpan  !ImageTag                  // (ImageYSpan t) is y-span of image tagged with t
  | TextXSpan   !FontDef !String           // (TextXSpan a b) is width of text b written in font a
  
  | PathXSpan   !ImageTag                  // (PathXSpan t) is x-span of path element tagged with t
  | PathYSpan   !ImageTag                  // (PathYSpan t) is y-span of path element tagged with t
:: FontDef`
  = { fontfamily`  :: !String              // font family name
    , fontysize`   :: !MilliInt            // font size as span (mpx -)
    , fontstretch` :: !String              // default value: "normal"
    , fontstyle`   :: !String              // default value: "normal"
    , fontvariant` :: !String              // default value: "normal"
    , fontweight`  :: !String              // default value: "normal"
    }
:: MilliInt
derive gEq        MilliInt
derive JSONEncode MilliInt
derive JSONDecode MilliInt
derive gText      MilliInt
derive gToJS      MilliInt

instance zero     MilliInt
instance one      MilliInt
instance ==       MilliInt
instance <        MilliInt
instance +        MilliInt
instance -        MilliInt
instance abs      MilliInt
instance ~        MilliInt
instance *        MilliInt
instance /        MilliInt
instance toReal   MilliInt
instance toString MilliInt

(*.) infixl 7 :: !Span !scalar -> Span | toReal scalar
(/.) infixl 7 :: !Span !scalar -> Span | toReal scalar

instance zero Span
instance +    Span
instance -    Span
instance abs  Span
instance ~    Span
instance *    Span
instance /    Span

isPxSpan    :: !Span -> Bool               // returns True only if argument is (PxSpan ...)

px          :: !a               -> Span | toMilliInt a
textxspan   :: !FontDef !String -> Span    // (textxspan font str) is the x-span of str written in font
imagexspan  :: !ImageTag        -> Span    // (imagexspan t) is x-span of image tagged with t
imageyspan  :: !ImageTag        -> Span    // (imageyspan t) is y-span of image tagged with t
columnspan  :: !ImageTag !Int   -> Span    // (columnspan t i) is x-span of column i (counting from 0) in grid tagged with t
rowspan     :: !ImageTag !Int   -> Span    // (rowspan    t i) is y-span of row    i (counting from 0) in grid tagged with t
minSpan     :: ![Span]          -> Span    // (minSpan as) is the minimum of as (zero if as = [])
maxSpan     :: ![Span]          -> Span    // (maxSpan as) is the maximum of as (zero if as = [])

:: ImageTag
  = ImageTagUser   !ImgTagNo !String
  | ImageTagSystem !ImgTagNo
:: ImgTagNo :== Int                        // internal numbering of (sub) images

instance == ImageTag
instance <  ImageTag

derive   gEq        FontDef`
derive   gPrint     FontDef`
derive   JSONEncode FontDef`
derive   JSONDecode FontDef`

setfontfamily`  :: !String   !FontDef` -> FontDef`
setfontysize`   :: !MilliInt !FontDef` -> FontDef`
setfontstretch` :: !String   !FontDef` -> FontDef`
setfontstyle`   :: !String   !FontDef` -> FontDef`
setfontvariant` :: !String   !FontDef` -> FontDef`
setfontweight`  :: !String   !FontDef` -> FontDef`
getfontfamily`  ::           !FontDef` -> String
getfontysize`   ::           !FontDef` -> MilliInt
getfontstretch` ::           !FontDef` -> String
getfontstyle`   ::           !FontDef` -> String
getfontvariant` ::           !FontDef` -> String
getfontweight`  ::           !FontDef` -> String

/** to2dec r:
		converts @r to a real value of two decimals in order to avoid errors in communicating real values
		with SVG clients
*/
to2dec :: !Real -> Real

class toMilliInt a :: !a -> MilliInt
instance toMilliInt MilliInt
instance toMilliInt Int
instance toMilliInt Real
instance toMilliInt Span	// this is a partial function! Only defined when isPxSpan returns True.
