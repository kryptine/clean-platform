implementation module Graphics.Scalable.Internal.Types

import iTasks.UI.JavaScript
import iTasks.Internal.Generic.Visualization
import Data.List
import Text
import Text.GenPrint
from StdOrdList import minList, maxList
import Graphics.Scalable.Types
import StdBool, StdInt, StdMisc, StdReal, StdString
from Text.GenJSON  import generic JSONEncode, generic JSONDecode, :: JSONNode

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
  = ColumnXSpan !ImageTag !Int             // (ColumnXSpan as a) is x-span of column number a in grid tagged with superset of as
  | RowYSpan    !ImageTag !Int             // (RowYSpan as a) is y-span of row number a in grid tagged with superset of as
  | ImageXSpan  !ImageTag                  // (ImageXSpan as) is x-span of image tagged with superset of as
  | ImageYSpan  !ImageTag                  // (ImageYSpan as) is y-span of image tagged with superset of as
  | TextXSpan   !FontDef !String           // (TextXSpan a b) is width of text b written in font a
  | PathXSpan   !ImageTag                  // (PathXSpan t) is x-span of path element tagged with t
  | PathYSpan   !ImageTag                  // (PathYSpan t) is y-span of path element tagged with t
:: FontDef`
  = { fontfamily`  :: !String              // font family name
    , fontysize`   :: !MilliInt            // font size as span (px -)
    , fontstretch` :: !String              // default value: "normal"
    , fontstyle`   :: !String              // default value: "normal"
    , fontvariant` :: !String              // default value: "normal"
    , fontweight`  :: !String              // default value: "normal"
    }
:: MilliInt
  = MilliInt !Int
derive gEq        MilliInt
derive gPrint     MilliInt
derive JSONEncode MilliInt
derive JSONDecode MilliInt
derive gText      MilliInt
gToJS{|MilliInt|} m = gToJS{|*|} (toReal m)

instance zero     MilliInt where zero                          = MilliInt 0
instance one      MilliInt where one                           = MilliInt 1000
instance ==       MilliInt where ==  (MilliInt a) (MilliInt b) = a == b
instance <        MilliInt where <   (MilliInt a) (MilliInt b) = a <  b
instance +        MilliInt where +   (MilliInt a) (MilliInt b) = MilliInt (a + b)
instance -        MilliInt where -   (MilliInt a) (MilliInt b) = MilliInt (a - b)
instance abs      MilliInt where abs (MilliInt a)              = MilliInt (abs a)
instance ~        MilliInt where ~   (MilliInt a)              = MilliInt (~ a)
instance *        MilliInt where *   a b                       = toMilliInt ((toReal a) * (toReal b))
instance /        MilliInt where /   a b                       = toMilliInt ((toReal a) / (toReal b))
instance toReal   MilliInt where toReal (MilliInt a)           = (toReal a) / 1000.0
instance toString MilliInt where toString (MilliInt a)
                                 | a <  0                      = concat ["-",toString (MilliInt (~a))]
                                 | a == 0                      = "0.0"
                                 | a < 1000                    = concat ["0.",toString a]
                                 | otherwise                   = concat [toString (a / 1000),".",toString (a rem 1000)]

(*.) infixl 7 :: !Span !scalar -> Span | toReal scalar
(*.) span scalar = (PxSpan (toMilliInt (toReal scalar))) * span

(/.) infixl 7 :: !Span !scalar -> Span | toReal scalar
(/.) span scalar = span / (PxSpan (toMilliInt (toReal scalar)))

/* The span computations below simplify their expressions by applying actual computations on (PxSpan _) values as much as possible.
   We denote a value (PxSpan _) with a capital letter. If we do not care or do not know, we denote the value with a lowercase letter.
   For instance: 
       (a * B) / C states that we know the values of B and C, but not of a. This can be simplified to:
       (B / C) * a
   In reflexive operations, we attempt to keep the (PxSpan _) values at the left operand.
   In the simplification functions, the first rule is always the 'eager' rule which applies the actual computation, and the last rule 
   is always the 'deferred' rule which only constructs the span expression lazily.
*/
instance / Span where
  / (PxSpan a)                (PxSpan b)                = PxSpan (a / b)
  / a=:(PxSpan z)             x
     | z == zero                                        = a                                            // ZERO / x              = ZERO
  / x                         (PxSpan o)
     | o == one                                         = x                                            // x / ONE               = x
  / (MulSpan (PxSpan b) a)    (MulSpan c (PxSpan d))
     | b == d                                           = a / c                                        // (B * a) / (c * B)     = a / c
  / (MulSpan a (PxSpan b))    (MulSpan c (PxSpan d))
     | b == d                                           = a / c                                        // (a * B) / (c * B)     = a / c
  / (MulSpan (PxSpan b) a)    (MulSpan (PxSpan d) c)
     | b == d                                           = a / c                                        // (B * a) / (B * c)     = a / c
  / (MulSpan a (PxSpan b))    (MulSpan (PxSpan d) c)
     | b == d                                           = a / c                                        // (a * B) / (B * c)     = a / c
  / (MulSpan (PxSpan b) a)    (PxSpan c)                = (PxSpan (b / c)) * a                         // (B * a) / C           = (B / C) * a
  / (MulSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b / c)) * a                         // (a * B) / C           = (B / C) * a
  / (DivSpan a (PxSpan b))    (PxSpan c)                = DivSpan a (PxSpan (b * c))                   // (a / B) / C           = a / (B * C)
  / (AddSpan (PxSpan a) b)    (PxSpan c)                = PxSpan (a / c) + b / (PxSpan c)              // (A + b) / C           = A / C + b / C
  / (SubSpan (PxSpan a) b)    (PxSpan c)                = PxSpan (a / c) - b / (PxSpan c)              // (A - b) / C           = A / C - b / C
  / (AddSpan a (PxSpan b))    (PxSpan c)                = PxSpan (b / c) + a / (PxSpan c)              // (a + B) / C           = B / C + a / C
  / (SubSpan a (PxSpan b))    (PxSpan c)                = PxSpan (zero - b / c) + a / (PxSpan c)       // (a - B) / C           = (0 - B / C) + a / C
  / l                         r                         = DivSpan l r

instance * Span where
  * (PxSpan a)                (PxSpan b)                = PxSpan (a * b)
  * a=:(PxSpan z)             x
     | z == zero                                        = a                                            // ZERO * x              = ZERO
  * x                         a=:(PxSpan z)
     | z == zero                                        = a                                            // x * ZERO              = ZERO
  * (PxSpan o)                x
     | o == one                                         = x                                            // ONE * x               = x
  * x                         (PxSpan o)
     | o == one                                         = x                                            // x * ONE               = x
  * (PxSpan a)                (MulSpan (PxSpan b) c)    = (PxSpan (a * b)) * c                         // A * (B * c)           = (A * B) * c
  * (PxSpan a)                (MulSpan b (PxSpan c))    = (PxSpan (a * c)) * b                         // A * (b * C)           = (A * C) * b
  * (MulSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a * c)) * b                         // (A * b) * C           = (A * C) * b
  * (MulSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b * c)) * a                         // (a * B) * C           = (B * C) * a
  * (DivSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a * c)) / b                         // (A / b) * C           = (A * C) / b
  * (DivSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (c / b)) * a                         // (a / B) * C           = (C / B) * a
  * (PxSpan c)                (DivSpan (PxSpan a) b)    = (PxSpan (c * a)) / b                         // C * (A / b)           = (C * A) / b
  * (PxSpan c)                (DivSpan a (PxSpan b))    = (PxSpan (c / b)) * a                         // C * (a / B)           = (C / B) * a
  * (PxSpan a)                (AddSpan (PxSpan b) c)    = (PxSpan (a * b)) + (PxSpan a) * c            // A * (B + c)           = A * B + A * c
  * (AddSpan (PxSpan b) c)    (PxSpan a)                = (PxSpan (a * b)) + (PxSpan a) * c            // (B + c) * A           = A * B + A * c
  * (PxSpan a)                (AddSpan c (PxSpan b))    = (PxSpan (a * b)) + (PxSpan a) * c            // A * (c + B)           = A * B + A * c
  * (AddSpan c (PxSpan b))    (PxSpan a)                = (PxSpan (a * b)) + (PxSpan a) * c            // (c + B) * A           = A * B + A * c
  * (PxSpan a)                (SubSpan (PxSpan b) c)    = (PxSpan (a * b)) - (PxSpan a) * c            // A * (B - c)           = A * B - A * c
  * (SubSpan (PxSpan b) c)    (PxSpan a)                = (PxSpan (a * b)) - (PxSpan a) * c            // (B - c) * A           = A * B - A * c
  * (PxSpan a)                (SubSpan c (PxSpan b))    = (PxSpan (zero - a * b)) + (PxSpan a) * c     // A * (c - B)           = (0 - A * B) + A * c
  * (SubSpan c (PxSpan b))    (PxSpan a)                = (PxSpan (zero - a * b)) + (PxSpan a) * c     // (c - B) * A           = (0 - A * B) + A * c
  * l                         r                         = MulSpan l r

instance + Span where
  + (PxSpan a)                (PxSpan b)                = PxSpan (a + b)
  + (PxSpan z)                x
     | z == zero                                        = x                                            // ZERO + x              = x
  + x                         (PxSpan z)
     | z == zero                                        = x                                            // x + ZERO              = x
  + (PxSpan a)                (AddSpan (PxSpan b) c)    = (PxSpan (a + b)) + c                         // A + (B + c)           = (A + B) + c
  + (PxSpan a)                (AddSpan b (PxSpan c))    = (PxSpan (a + c)) + b                         // A + (b + C)           = (A + C) + b
  + (AddSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a + c)) + b                         // (A + b) + C           = (A + C) + b
  + (AddSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b + c)) + a                         // (a + B) + C           = (B + C) + a
  + (SubSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a + c)) - b                         // (A - b) + C           = (A + C) - b
  + (SubSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (c - b)) + a                         // (a - B) + C           = (C - B) + a
  + (PxSpan a)                (SubSpan b (PxSpan c))    = (PxSpan (a - c)) + b                         // A + (b - C)           = (A - C) + b
  + (PxSpan a)                (SubSpan (PxSpan b) c)    = (PxSpan (a + b)) - c                         // A + (B - c)           = (A + B) - c
  + (DivSpan a l=:(PxSpan b)) (DivSpan c r=:(PxSpan d))
     | b == d                                           = DivSpan (a + c) l                            // (a / B) + (c / B)     = (a + c) / B
  + (MulSpan p=:(PxSpan a) b) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (b + d)                                  // A * b + A * d         = A * (b + d)
  + (MulSpan b p=:(PxSpan a)) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (d + b)                                  // b * A + A * d         = A * (d + b)
  + (MulSpan p=:(PxSpan a) b) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b + d)                                  // A * b + d * A         = A * (b + d)
  + (MulSpan b p=:(PxSpan a)) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b + d)                                  // b * A + d * A         = A * (b + d)
  + s                         t                         = AddSpan s t

instance - Span where
  - (PxSpan a)                (PxSpan b)                = PxSpan (a - b)
  - a                         (PxSpan z)
     | z == zero                                        = a                                            // a - ZERO              = a
  - (SubSpan (PxSpan b) a)    (PxSpan c)                = (PxSpan (b - c)) - a                         // (B - a) - C           = (B - C) - a
  - (SubSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (zero - b - c)) + a                  // (a - B) - C           = (0 - B - C) + a
  - (PxSpan c)                (SubSpan a (PxSpan b))    = (PxSpan (c + b)) - a                         // C - (a - B)           = (C + B) - a
  - (PxSpan c)                (SubSpan (PxSpan b) a)    = (PxSpan (c - b)) + a                         // C - (B - a)           = (C - B) + a
  - (AddSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a - c)) + b                         // (A + b) - C           = (A - C) + b
  - (AddSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b - c)) + a                         // (a + B) - C           = (B - C) + a
  - (PxSpan c)                (AddSpan a (PxSpan b))    = (PxSpan (c - b)) - a                         // C - (a + B)           = (C - B) - a
  - (PxSpan c)                (AddSpan (PxSpan a) b)    = (PxSpan (c - a)) - b                         // C - (A + b)           = (C - A) - b
  - (DivSpan a l=:(PxSpan b)) (DivSpan c r=:(PxSpan d))
     | b == d                                           = DivSpan (a - c) l                            // (a / B) - (c / B)     = (a - c) / B
  - (MulSpan p=:(PxSpan a) b) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (b - d)                                  // A * b - A * d         = A * (b - d)
  - (MulSpan b p=:(PxSpan a)) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (b - d)                                  // b * A - A * d         = A * (b - d)
  - (MulSpan p=:(PxSpan a) b) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b - d)                                  // A * b - d * A         = A * (b - d)
  - (MulSpan b p=:(PxSpan a)) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b - d)                                  // b * A - d * A         = A * (b - d)
  - s                         t                         = SubSpan s t


instance zero Span where zero                           = PxSpan zero

instance abs  Span where abs (PxSpan  x)                = PxSpan (abs x)
                         abs (AbsSpan x)                = AbsSpan x
                         abs span                       = AbsSpan span

instance ~    Span where ~   s                          = zero - s

isPxSpan :: !Span -> Bool
isPxSpan (PxSpan _) = True
isPxSpan _          = False

getPxSpan :: !Span -> MilliInt
getPxSpan (PxSpan r) = r
getPxSpan _          = abort "Fatal error in module Graphics.Scalable.Internal.Types: getPxSpan applied to illegal argument"

mpx :: !n -> Span | toMilliInt n
mpx n = PxSpan (toMilliInt n)

textxspan :: !FontDef !String -> Span
textxspan a b = LookupSpan (TextXSpan a b)

imagexspan :: !ImageTag -> Span
imagexspan t = LookupSpan (ImageXSpan t)

imageyspan :: !ImageTag -> Span
imageyspan t = LookupSpan (ImageYSpan t)

columnspan :: !ImageTag !Int -> Span
columnspan t a = LookupSpan (ColumnXSpan t a)

rowspan :: !ImageTag !Int -> Span
rowspan t a = LookupSpan (RowYSpan t a)

minSpan :: ![Span] -> Span
minSpan []  = zero
minSpan [x] = x
minSpan spans
  #! spans` = flattenMinSpans spans []
  = case partition isPxSpan spans` of
      (pxs, [])     -> minPxs pxs
      ([], others)  -> MinSpan others
      (pxs, others) -> MinSpan [minPxs pxs : others]
  where
  minPxs :: ![Span] -> Span
  minPxs pxs = PxSpan (minList [x \\ PxSpan x <- pxs])

  flattenMinSpans :: ![Span] ![Span] -> [Span]
  flattenMinSpans []              acc = acc
  flattenMinSpans [MinSpan os:xs] acc = flattenMinSpans xs (os ++ acc)
  flattenMinSpans [x:xs]          acc = flattenMinSpans xs [x:acc]

maxSpan :: ![Span] -> Span
maxSpan []  = zero
maxSpan [x] = x
maxSpan spans
  #! spans` = flattenMaxSpans spans []
  = case partition isPxSpan spans` of
      (pxs, [])     -> maxPxs pxs
      ([], others)  -> MaxSpan others
      (pxs, others) -> MaxSpan [maxPxs pxs : others]
  where
  maxPxs :: ![Span] -> Span
  maxPxs pxs = PxSpan (maxList [x \\ PxSpan x <- pxs])

  flattenMaxSpans :: ![Span] ![Span] -> [Span]
  flattenMaxSpans []              acc = acc
  flattenMaxSpans [MaxSpan os:xs] acc = flattenMaxSpans xs (os ++ acc)
  flattenMaxSpans [x:xs]          acc = flattenMaxSpans xs [x:acc]

instance == ImageTag where == (ImageTagUser n1 s1) (ImageTagUser n2 s2) = n1 == n2 && s1 == s2
                           == (ImageTagSystem  s1) (ImageTagSystem  s2) = s1 == s2
                           == _                    _                    = False
instance <  ImageTag where <  (ImageTagUser n1 s1) (ImageTagUser n2 s2) = n1 < n2 || (n1 == n2 && s1 < s2)
                           <  (ImageTagUser _  _)  _                    = True
                           <  (ImageTagSystem  s1) (ImageTagSystem  s2) = s1 < s2
                           <  _                    _                    = False

derive gEq        FontDef`
derive JSONEncode FontDef`
derive JSONDecode FontDef`
derive gPrint     FontDef`


setfontfamily` :: !String !FontDef` -> FontDef`
setfontfamily` family fontdef = {FontDef` | fontdef & fontfamily` = family}

setfontysize` :: !MilliInt !FontDef` -> FontDef`
setfontysize` ysize fontdef = {FontDef` | fontdef & fontysize` = ysize}

setfontstretch` :: !String !FontDef` -> FontDef`
setfontstretch` stretch fontdef = {FontDef` | fontdef & fontstretch` = stretch}

setfontstyle` :: !String !FontDef` -> FontDef`
setfontstyle` style fontdef = {FontDef` | fontdef & fontstyle` = style}

setfontvariant` :: !String !FontDef` -> FontDef`
setfontvariant` variant fontdef = {FontDef` | fontdef & fontvariant` = variant}

setfontweight` :: !String !FontDef` -> FontDef`
setfontweight` weight fontdef = {FontDef` | fontdef & fontweight` = weight}

getfontfamily` :: !FontDef` -> String
getfontfamily` {FontDef` | fontfamily`} = fontfamily`

getfontysize` :: !FontDef` -> MilliInt
getfontysize` {FontDef` | fontysize`} = fontysize`

getfontstretch` :: !FontDef` -> String
getfontstretch` {FontDef` | fontstretch`} = fontstretch`

getfontstyle` :: !FontDef` -> String
getfontstyle` {FontDef` | fontstyle`} = fontstyle`

getfontvariant` :: !FontDef` -> String
getfontvariant` {FontDef` | fontvariant`} = fontvariant`

getfontweight` :: !FontDef` -> String
getfontweight` {FontDef` | fontweight`} = fontweight`

class toMilliInt a :: !a -> MilliInt
instance toMilliInt MilliInt where toMilliInt n = n
instance toMilliInt Int      where toMilliInt n = MilliInt        (n * 1000)
instance toMilliInt Real     where toMilliInt r = MilliInt (toInt (r * 1000.0))
instance toMilliInt Span     where toMilliInt (PxSpan n) = n
                                   toMilliInt _ = abort "toMilliInt: applied to value other than PxSpan"

to2dec :: !Real -> Real
to2dec r = toReal (toInt (r * 100.0)) / 100.0
