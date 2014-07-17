definition module Graphics.Scalable

/** A proposal for a compositional image library for defining SVG-images.
	Peter Achten
	6-6-2014
*/
from Data.Maybe import :: Maybe
from Text.HTML import :: SVGColor
from StdOverloaded import class zero, class +, class -, class ~, class one, class abs

:: Image m
	= { content   :: ImageContent m		// the image elements
	  , attribs   :: [ImageAttr m]			// the image attributes
	  , transform :: [ImageTransform]		// [t_1, ..., t_n] transforms the image as t_1 o ... o t_n
	  , tags      :: [ImageTag]			// sorted list of tags
	  }
:: ImageTransform
	= RotateImage ImageAngle
	| SkewXImage  ImageAngle
	| SkewYImage  ImageAngle
	| FitImage    Span Span
	| FitXImage   Span
	| FitYImage   Span
:: ImageContent m
	= Basic     BasicImage ImageSpan
	| Composite (CompositeImage m)
:: Span
	= PxSpan       Real					// (PxSpan a) is a pixels
	| LookupSpan   LookupSpan				// (LookupSpan a) needs to be looked up after computing dimensions
	| AddSpan      Span Span				// (AddSpan a b) is span a + span b
	| SubSpan      Span Span				// (SubSpan a b) is span a - span b
	| MulSpan      Span Real				// (MulSpan a k) is (span a) * k
	| DivSpan      Span Real				// (DivSpan a k) is (span a) / k
	| AbsSpan      Span					// (AbsSpan a)  is absolute value of span a
	| MinSpan      [Span]					// (MinSpan as) is minimum span value in as
	| MaxSpan      [Span]					// (MaxSpan as) is maximum span value in as
:: ImageSpan
	= { xspan     :: Span
	  , yspan     :: Span
	  }
:: BasicImage
	= EmptyImage
	| TextImage FontDef String
	| LineImage Slash
	| CircleImage
	| RectImage
	| EllipseImage
:: CompositeImage m
	= { offsets   :: [ImageOffset]
	  , content   :: [Image m]
	  , host      :: Host m
	  , compose   :: Compose
	  }
:: LookupSpan
	= ColumnXSpan  [ImageTag] Int			// (ColumnXSpan as a) is x-span of column number a in grid tagged with superset of as
	| DescentYSpan FontDef					// (DescentYSpan a) is descent height of font a
	| ExYSpan      FontDef					// (ExYSpan a) is ex height of font a
	| ImageXSpan   [ImageTag]				// (ImageXSpan as) is x-span of image tagged with superset of as
	| ImageYSpan   [ImageTag]				// (ImageYSpan as) is y-span of image tagged with superset of as
	| RowYSpan     [ImageTag] Int			// (RowYSpan as a) is y-span of row number a in grid tagged with superset of as
	| TextXSpan    FontDef String			// (TextXSpan a b) is width of text b written in font a
:: Compose
	= AsGrid Int [ImageAlign]				// (AsGrid nr_of_rows alignments) composes elements in rows, using alignments per image
	| AsCollage								// AsCollage composes elements in freestyle, framed in optional host
	| AsOverlay   [ImageAlign]				// AsOverlay composes elements, framed in optional host or largest spans

px			:: Real            -> Span		// (px a) is a pixels
ex			:: FontDef         -> Span		// (ex font) is the ex height (ascent) of font
descent		:: FontDef         -> Span		// (descent font) is the descent height of font
textxspan	:: FontDef String -> Span		// (textxspan font str) is the x-span of str written in font
imagexspan	:: [ImageTag]      -> Span		// (imagexspan ts) is x-span of image tagged with superset of ts
imageyspan  :: [ImageTag]      -> Span		// (imageyspan ts) is y-span of image tagged with superset of ts
columnspan	:: [ImageTag] Int -> Span		// (columnspan ts i) is x-span of column i in grid tagged with superset of ts
rowspan		:: [ImageTag] Int -> Span		// (rowspan ts i) is y-span of row i in grid tagged with superset of ts

class (*.) infixl 7 a :: Span a -> Span
class (/.) infixl 7 a :: Span a -> Span

instance zero Span
instance one  Span
instance +    Span
instance -    Span
instance abs  Span
instance ~    Span
instance *.   Int, Real
instance /.   Int, Real

minSpan :: [Span] -> Span					// (minimum as) is the minimum of as (zero if as = [])
maxSpan :: [Span] -> Span					// (maximum as) is the maximum of as (zero if as = [])

:: FontDef
	= { fontfamily  :: String
	  , fontyspan   :: Span
	  , fontstretch :: String
	  , fontstyle   :: String
	  , fontvariant :: String
	  , fontweight  :: String
	  }

empty	:: Span Span            -> Image m		// (empty a b) is an empty image with x-span a and y-span b
text	:: FontDef String       -> Image m		// (text font str) is an image containg str written in font
xline	:: Span                 -> Image m		// (xline a) is an image of a line with x-span a and y-span zero
yline	:: Span                 -> Image m		// (yline a) is an image of a line with y-span a and x-span zero
line	:: Slash Span Span      -> Image m		// (line a b) is an image of a line with x-span a and y-span b
circle	:: Span                 -> Image m		// (circle a) is an image of a circle with diameter a
ellipse	:: Span Span            -> Image m		// (ellipse a b) is an image of an ellipse with x-diameter a and y-diameter b
rect	:: Span Span            -> Image m		// (rect a b) is an image of a rectangle with x-span a and y-span b

rotate  :: ImageAngle (Image m) -> Image m
fit     :: Span Span (Image m)  -> Image m
fitx    :: Span       (Image m) -> Image m
fity    :: Span       (Image m) -> Image m
skewx   :: ImageAngle (Image m) -> Image m
skewy   :: ImageAngle (Image m) -> Image m

:: Slash = Slash | Backslash

:: ImageAngle
	:== Real

radian  :: Real -> ImageAngle
degree  :: Real -> ImageAngle

:: Host m :== Maybe (Image m)

overlay ::                          [ImageAlign] [ImageOffset] [Image m] (Host m) -> Image m
beside  ::                              [YAlign] [ImageOffset] [Image m] (Host m) -> Image m
above   ::                              [XAlign] [ImageOffset] [Image m] (Host m) -> Image m
grid    :: GridDimension GridLayout [ImageAlign] [ImageOffset] [Image m] (Host m) -> Image m
collage ::                                       [ImageOffset] [Image m] (Host m) -> Image m

:: XAlign
	= AtLeft | AtMiddleX | AtRight
:: YAlign
	= AtTop | AtMiddleY | AtBottom
:: ImageAlign  :== (XAlign, YAlign)
:: ImageOffset :== (Span, Span)
:: GridDimension = Rows Int | Columns Int
:: GridLayout  :== (GridXLayout, GridYLayout)
:: GridXLayout   = LeftToRight | RightToLeft
:: GridYLayout   = TopToBottom | BottomToTop

:: ImageAttr m
	= ImageStrokeAttr      (StrokeAttr      m)
	| ImageStrokeWidthAttr (StrokeWidthAttr m)
	| ImageFillAttr        (FillAttr        m)
	| ImageOpacityAttr     (OpacityAttr     m)
	| ImageOnClickAttr     (OnClickAttr     m)


class tune_image attr :: (Image m) (attr m) -> Image m
(<@<) infixl 2 :: (Image m) (attr m) -> Image m | tune_image attr
(>@>) infixr 2 :: (attr m) (Image m) -> Image m | tune_image attr

:: StrokeAttr      m = { stroke      :: SVGColor }
:: StrokeWidthAttr m = { strokewidth :: Span     }
:: FillAttr        m = { fill        :: SVGColor }
:: OpacityAttr     m = { opacity     :: Real     }
:: OnClickAttr     m = { onclick     :: (m -> m) }

instance tune_image StrokeAttr, StrokeWidthAttr, FillAttr, OpacityAttr, OnClickAttr

class toSVGColor a :: a -> SVGColor
instance toSVGColor String, RGB
:: RGB = { r :: Int, g :: Int, b :: Int }

:: ImageTag
	= ImageTagInt    Int
	| ImageTagString String
	| ImageTagSystem Int
class imageTag a :: a -> ImageTag
instance imageTag Int
instance imageTag String

tag  :: [ImageTag] (Image m) -> Image m
tags :: (Image m) -> [ImageTag]
