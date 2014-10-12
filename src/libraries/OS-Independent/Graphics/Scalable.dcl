definition module Graphics.Scalable

/** A proposal for a compositional image library for defining SVG-images.
	Peter Achten
	6-6-2014
*/
from Data.Maybe import :: Maybe
from Text.HTML import :: SVGColor
from Data.Set import :: Set
from StdOverloaded import class zero, class +, class -, class ~, class one, class sign, class abs, class <, class ==, class toReal, class /, class *

:: Image m
  = { content             :: ImageContent m           // the image elements
    , mask                :: Maybe (Image m)          // the mask image
    , attribs             :: Set (ImageAttr m)        // the image attributes
    , transform           :: [ImageTransform]         // [t_1, ..., t_n] transforms the image as t_1 o ... o t_n
    , tags                :: Set ImageTag             // set of tags
    , totalSpan           :: ImageSpan                // Total image span
    , margin              :: (Span, Span, Span, Span) // Image margin
    , transformCorrection :: ImageOffset              // Correction required after transformation
    , connectors          :: [Connector]              // Connector point to which lines may be attached
    }

:: ImageTransform
  = RotateImage Deg
  | SkewXImage  Deg
  | SkewYImage  Deg
  | FitImage    Span Span
  | FitXImage   Span
  | FitYImage   Span

:: ImageContent m
  = Basic     BasicImage ImageSpan
  | Line      (LineImage m)
  | Composite (CompositeImage m)

:: LineImage m
  = { lineSpan    :: ImageSpan
    , markers     :: Maybe (Markers m)
    , lineContent :: LineContent
    }

:: Markers m
  = { markerStart :: Maybe (Image m)
    , markerMid   :: Maybe (Image m)
    , markerEnd   :: Maybe (Image m)
    }

:: LineContent
  = SimpleLineImage Slash
  | PolygonImage    [ImageOffset]
  | PolylineImage   [ImageOffset]
  //| PathImage       [PathSegment]

//:: PathSegment
  //= MMoveTo                       ImageOffset
  //| LLineTo                       ImageOffset
  //| HHorizontalLineTo             Span
  //| VVerticalLineTo               Span
  //| CCurveTo                      ImageOffset ImageOffset ImageOffset
  //| SSmoothCurveTo                ImageOffset ImageOffset
  //| QQuadraticBezierCurve         ImageOffset ImageOffset
  //| TSmoothQuadraticBezierCurveTo ImageOffset
  //| AEllipticalArc                ImageSpan Deg Bool Bool ImageOffset
  //| ZClosePath

:: Span
  = PxSpan     Real       // (PxSpan a) is a pixels
  | LookupSpan LookupSpan // (LookupSpan a) needs to be looked up after computing dimensions
  | AddSpan    Span Span  // (AddSpan a b) is span a + span b
  | SubSpan    Span Span  // (SubSpan a b) is span a - span b
  | MulSpan    Span Span  // (MulSpan a b) is span a * span k
  | DivSpan    Span Span  // (DivSpan a b) is span a / span k
  | AbsSpan    Span       // (AbsSpan a)  is absolute value of span a
  | MinSpan    [Span]     // (MinSpan as) is minimum span value in as
  | MaxSpan    [Span]     // (MaxSpan as) is maximum span value in as

:: ImageSpan :== (Span, Span)

:: Connector :== (Span, Span)

:: Edge :== ([ImageTag], [ImageTag])

:: BasicImage
  = EmptyImage
  | TextImage FontDef String
  | CircleImage
  | RectImage
  | EllipseImage

:: CompositeImage m
  = { offsets :: [ImageOffset]
    , host    :: Host m
    , compose :: Compose m
    , edges   :: Set (Set ImageTag, Set ImageTag)
    }

:: LookupSpan
  = ColumnXSpan  (Set ImageTag) Int // (ColumnXSpan as a) is x-span of column number a in grid tagged with superset of as
  | RowYSpan     (Set ImageTag) Int // (RowYSpan as a) is y-span of row number a in grid tagged with superset of as
  | ImageXSpan   (Set ImageTag)     // (ImageXSpan as) is x-span of image tagged with superset of as
  | ImageYSpan   (Set ImageTag)     // (ImageYSpan as) is y-span of image tagged with superset of as
  | TextXSpan    FontDef String     // (TextXSpan a b) is width of text b written in font a

:: Compose m
  = AsGrid    (Int, Int) [ImageAlign] [[Image m]] // (AsGrid (noOfCols, noOfRows) alignments) composes elements in rows, using alignments per image
  | AsCollage                         [Image m]   // AsCollage composes elements in freestyle, framed in optional host
  | AsOverlay            [ImageAlign] [Image m]   // AsOverlay composes elements, framed in optional host or largest spans

px         :: !Real            -> Span // (px a) is a pixels
textxspan  :: !FontDef !String -> Span // (textxspan font str) is the x-span of str written in font
imagexspan :: ![ImageTag]      -> Span // (imagexspan ts) is x-span of image tagged with superset of ts
imageyspan :: ![ImageTag]      -> Span // (imageyspan ts) is y-span of image tagged with superset of ts
columnspan :: ![ImageTag] !Int -> Span // (columnspan ts i) is x-span of column i in grid tagged with superset of ts
rowspan    :: ![ImageTag] !Int -> Span // (rowspan ts i) is y-span of row i in grid tagged with superset of ts

class (*.) infixl 7 a :: a n -> a | toReal n
class (/.) infixl 7 a :: a n -> a | toReal n

instance zero Span
instance one  Span
instance +    Span
instance -    Span
instance abs  Span
instance ~    Span
instance *.   Span, Real, Int
instance /.   Span, Real, Int

class maxOf a :: [a] -> a
class minOf a :: [a] -> a

instance maxOf Span, Real, Int
instance minOf Span, Real, Int

class IsSpan a | zero a & one a & + a & - a & abs a & ~ a & *. a & /. a & maxOf a & minOf a where
  toSpan :: a -> Span

instance IsSpan Int
instance IsSpan Real
instance IsSpan Span

minSpan :: ![s] -> Span | IsSpan s // (minimum as) is the minimum of as (zero if as = [])
maxSpan :: ![s] -> Span | IsSpan s // (maximum as) is the maximum of as (zero if as = [])

class margin a where
  margin :: !a !(Image m) -> Image m

instance margin Span // Margin is the same span on all sides
instance margin Int  // Margin is the same span on all sides
instance margin Real // Margin is the same span on all sides
instance margin (a, b)       | IsSpan a & IsSpan b                       // (h, v) Margin is h on top and bottom and v on left and right
instance margin (a, b, c)    | IsSpan a & IsSpan b & IsSpan c            // (t, h, b) Margin is t on top, v on left and right and b on bottom
instance margin (a, b, c, d) | IsSpan a & IsSpan b & IsSpan c & IsSpan d // (t, r, b, l) Margin is t on top, r on the right, b on the bottom and l on the left

:: FontDef
  = { fontfamily  :: String
    , fontysize   :: Real
    , fontstretch :: String
    , fontstyle   :: String
    , fontvariant :: String
    , fontweight  :: String
    }

empty    :: !s !s            -> Image m | IsSpan s // (empty a b) is an empty image with x-span a and y-span b
text     :: !FontDef !String -> Image m            // (text font str) is an image containg str written in font
circle   :: !s               -> Image m | IsSpan s // (circle a) is an image of a circle with diameter a
ellipse  :: !s !s            -> Image m | IsSpan s // (ellipse a b) is an image of an ellipse with x-diameter a and y-diameter b
rect     :: !s !s            -> Image m | IsSpan s // (rect a b) is an image of a rectangle with x-span a and y-span b

xline    :: !(Maybe (Markers m)) !s             -> Image m | IsSpan s // (xline a) is an image of a line with x-span a and y-span zero
yline    :: !(Maybe (Markers m)) !s             -> Image m | IsSpan s // (yline a) is an image of a line with y-span a and x-span zero
line     :: !(Maybe (Markers m)) !Slash !s !s   -> Image m | IsSpan s // (line a b) is an image of a line with x-span a and y-span b
polygon  :: !(Maybe (Markers m)) ![ImageOffset] -> Image m // (polygon xs) is an image of a polygon with coordinates xs
polyline :: !(Maybe (Markers m)) ![ImageOffset] -> Image m // (polyline xs) is an image of a polyline with coordinates xs

rotate  :: !th   !(Image m) -> Image m | Angle th
fit     :: !s !s !(Image m) -> Image m | IsSpan s
fitx    :: !s    !(Image m) -> Image m | IsSpan s
fity    :: !s    !(Image m) -> Image m | IsSpan s
skewx   :: !th   !(Image m) -> Image m | Angle th
skewy   :: !th   !(Image m) -> Image m | Angle th

:: Slash = Slash | Backslash

radian :: !Real -> Rad
degree :: !Real -> Deg

:: Host m :== Maybe (Image m)

overlay ::                            ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
beside  ::                                ![YAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
above   ::                                ![XAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid    :: !GridDimension !GridLayout ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
collage ::                                          ![ImageOffset] ![Image m] !(Host m) -> Image m

(@$) infixr :: !(Image m) !(Image m) -> Image m
($@) infixl :: !(Image m) !(Image m) -> Image m

maskWith :: !(Image m) !(Image m) -> Image m

addEdge :: ![ImageTag] ![ImageTag] !(Image m) -> Image m

:: XAlign
  = AtLeft
  | AtMiddleX
  | AtRight

:: YAlign
  = AtTop
  | AtMiddleY
  | AtBottom

:: ImageAlign  :== (XAlign, YAlign)
:: ImageOffset :== (Span, Span)
:: GridDimension = Rows Int | Columns Int
:: GridLayout  :== (GridXLayout, GridYLayout)
:: GridXLayout   = LeftToRight | RightToLeft
:: GridYLayout   = TopToBottom | BottomToTop

:: ImageAttr m
  = ImageStrokeAttr        (StrokeAttr      m)
  | ImageStrokeWidthAttr   (StrokeWidthAttr m)
  | ImageXRadiusAttr       (XRadiusAttr     m)
  | ImageYRadiusAttr       (YRadiusAttr     m)
  | ImageStrokeOpacityAttr (OpacityAttr     m)
  | ImageFillAttr          (FillAttr        m)
  | ImageFillOpacityAttr   (OpacityAttr     m)
  | ImageOnClickAttr       (OnClickAttr     m)
  | ImageDashAttr          (DashAttr        m)

instance <  (ImageAttr m)
instance == (ImageAttr m)

class tuneImage attr :: (Image m) (attr m) -> Image m
(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tuneImage attr
(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tuneImage attr

:: StrokeAttr        m = { stroke      :: SVGColor }
:: StrokeWidthAttr   m = { strokewidth :: Span     }
:: XRadiusAttr       m = { xradius     :: Span     }
:: YRadiusAttr       m = { yradius     :: Span     }
:: FillAttr          m = { fill        :: SVGColor }
:: OpacityAttr       m = { opacity     :: Real     }
:: OnClickAttr       m = { onclick     :: (m -> m) }
:: DashAttr          m = { dash        :: [Int]    }

instance tuneImage StrokeAttr, StrokeWidthAttr, FillAttr, OpacityAttr, OnClickAttr, XRadiusAttr, YRadiusAttr, DashAttr

class toSVGColor a :: a -> SVGColor
instance toSVGColor String, RGB
instance zero RGB

:: RGB = { r :: Int, g :: Int, b :: Int }

:: ImageTag
  = ImageTagInt    Int
  | ImageTagString String
  | ImageTagSystem Int

class imageTag a :: a -> ImageTag
instance imageTag Int
instance imageTag String

tag  :: ![ImageTag] !(Image m) -> Image m
tags :: !(Image m) -> [ImageTag]

instance == ImageTag
instance <  ImageTag

instance +  ImageOffset

:: Deg = Deg Real
:: Rad = Rad Real

class Angle a where
  toDeg     :: a -> Deg
  toRad     :: a -> Rad
  normalize :: a -> a

instance toReal Deg
instance toReal Rad
instance Angle  Deg
instance Angle  Rad
instance sign   Deg
instance sign   Rad

isPxSpan :: !Span -> Bool

instance == FontDef
instance < FontDef
