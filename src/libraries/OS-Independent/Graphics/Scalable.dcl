definition module Graphics.Scalable

/** A proposal for a compositional image library for defining SVG-images.
	Peter Achten
	6-6-2014
*/
from Data.Maybe import :: Maybe
from Text.HTML import :: SVGColor
from StdOverloaded import class zero, class +, class -, class ~, class one, class abs

:: Image m
:: Span

px			:: !Real            -> Span		// (px a) is a pixels
ex			:: !FontDef         -> Span		// (ex font) is the ex height (ascent) of font
descent		:: !FontDef         -> Span		// (descent font) is the descent height of font
textxspan	:: !FontDef !String -> Span		// (textxspan font str) is the x-span of str written in font
imagexspan	:: ![ImageTag]      -> Span		// (imagexspan ts) is x-span of image tagged with superset of ts
imageyspan  :: ![ImageTag]      -> Span		// (imageyspan ts) is y-span of image tagged with superset of ts
columnspan	:: ![ImageTag] !Int -> Span		// (columnspan ts i) is x-span of column i in grid tagged with superset of ts
rowspan		:: ![ImageTag] !Int -> Span		// (rowspan ts i) is y-span of row i in grid tagged with superset of ts

class (*.) infixl 7 a :: !Span !a -> Span
class (/.) infixl 7 a :: !Span !a -> Span

instance zero Span
instance one  Span
instance +    Span
instance -    Span
instance abs  Span
instance ~    Span
instance *.   Int, Real
instance /.   Int, Real

minSpan :: ![Span] -> Span					// (minimum as) is the minimum of as (zero if as = [])
maxSpan :: ![Span] -> Span					// (maximum as) is the maximum of as (zero if as = [])

:: FontDef
	= { fontfamily  :: !String
	  , fontyspan   :: !Span
	  , fontstretch :: !String
	  , fontstyle   :: !String
	  , fontvariant :: !String
	  , fontweight  :: !String
	  }

empty	:: !Span !Span            -> Image m		// (empty a b) is an empty image with x-span a and y-span b
text	:: !FontDef !String       -> Image m		// (text font str) is an image containg str written in font
xline	:: !Span                  -> Image m		// (xline a) is an image of a line with x-span a and y-span zero
yline	:: !Span                  -> Image m		// (yline a) is an image of a line with y-span a and x-span zero
line	:: !Slash !Span !Span     -> Image m		// (line a b) is an image of a line with x-span a and y-span b
circle	:: !Span                  -> Image m		// (circle a) is an image of a circle with diameter a
ellipse	:: !Span !Span            -> Image m		// (ellipse a b) is an image of an ellipse with x-diameter a and y-diameter b
rect	:: !Span !Span            -> Image m		// (rect a b) is an image of a rectangle with x-span a and y-span b

rotate  :: !ImageAngle !(Image m) -> Image m
fit     :: !Span !Span !(Image m) -> Image m
fitx    :: !Span       !(Image m) -> Image m
fity    :: !Span       !(Image m) -> Image m
skewx   :: !ImageAngle !(Image m) -> Image m
skewy   :: !ImageAngle !(Image m) -> Image m

:: Slash = Slash | Backslash

:: ImageAngle
radian  :: !Real -> ImageAngle
degree  :: !Real -> ImageAngle

:: Host m :== Maybe (Image m)

overlay ::                            ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
beside  ::                                ![YAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
above   ::                                ![XAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid    :: !GridDimension !GridLayout ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
collage ::                                          ![ImageOffset] ![Image m] !(Host m) -> Image m

:: XAlign
	= AtLeft | AtMiddleX | AtRight
:: YAlign
	= AtTop | AtMiddleY | AtBottom
:: ImageAlign  :== (XAlign, YAlign)
:: ImageOffset :== (Span, Span)
:: GridDimension = Rows !Int | Columns !Int
:: GridLayout  :== (GridXLayout, GridYLayout)
:: GridXLayout   = LeftToRight | RightToLeft
:: GridYLayout   = TopToBottom | BottomToTop

:: ImageAttr m

class tune_image attr :: !(Image m) !(attr m) -> Image m
(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tune_image attr
(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tune_image attr

:: StrokeAttr      m = { stroke      :: !SVGColor }
:: StrokeWidthAttr m = { strokewidth :: !Span     }
:: FillAttr        m = { fill        :: !SVGColor }
:: OpacityAttr     m = { opacity     :: !Real     }
:: OnClickAttr     m = { onclick     :: !(m -> m) }

instance tune_image StrokeAttr, StrokeWidthAttr, FillAttr, OpacityAttr, OnClickAttr

class toSVGColor a :: !a -> SVGColor
instance toSVGColor String, RGB
:: RGB = { r :: !Int, g :: !Int, b :: !Int }

:: ImageTag
class imageTag a :: !a -> ImageTag
instance imageTag Int
instance imageTag String

tag  :: ![ImageTag] !(Image m) -> Image m
tags :: !(Image m) -> [ImageTag]
