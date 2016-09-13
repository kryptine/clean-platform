definition module Graphics.Scalable

/** A proposal for a compositional image library for defining SVG-images.
	Peter Achten
	Jurrien Stutterheim
	Rinus Plasmeijer
*/
from Graphics.Scalable.Internal import :: Image, :: ImageTag, :: Span,
  :: FontDef {..}, :: ImageOffset, :: Angle, :: Markers {..}, :: ImageAttr,
  :: StrokeAttr {..}, :: StrokeWidthAttr {..}, :: XRadiusAttr {..},
  :: YRadiusAttr {..}, :: FillAttr {..}, :: OpacityAttr {..},
  :: OnClickAttr {..}, :: OnMouseDownAttr {..}, :: OnMouseUpAttr {..},
  :: OnMouseOverAttr {..}, :: OnMouseMoveAttr {..}, :: OnMouseOutAttr {..},
  :: DraggableAttr {..}, :: DashAttr {..}, :: NoAttr (..),
  :: MaskAttr {..}, :: Slash (..), :: ImageAlign, :: XAlign (..), :: YAlign (..),
  :: Host, :: GridLayout, :: GridMajor(..), :: GridXLayout (..), :: GridYLayout (..), :: GridDimension (..),
  class /. (..), class *. (..), instance *. Span, instance /. Span,
  instance + Span, instance - Span, instance zero Span, instance ~ Span
from StdOverloaded import class zero (..), class + (..), class - (..),
  class ~ (..), class sign (..), class abs (..), class < (..), class == (..),
  class toReal (..), class / (..), class * (..)
from Data.Maybe import :: Maybe (..)
from Text.HTML import :: SVGColor
from Data.Set import :: Set
import Math.Geometry

px         :: !Real            -> Span // (px a) is a pixels
textxspan  :: !FontDef !String -> Span // (textxspan font str) is the x-span of str written in font
imagexspan :: !ImageTag        -> Span // (imagexspan ts) is x-span of image tagged with superset of ts
imageyspan :: !ImageTag        -> Span // (imageyspan ts) is y-span of image tagged with superset of ts
columnspan :: !ImageTag !Int   -> Span // (columnspan ts i) is x-span of column i in grid tagged with superset of ts
rowspan    :: !ImageTag !Int   -> Span // (rowspan ts i) is y-span of row i in grid tagged with superset of ts

minSpan :: ![Span] -> Span // (minimum as) is the minimum of as (zero if as = [])

maxSpan :: ![Span] -> Span // (maximum as) is the maximum of as (zero if as = [])

class margin a where
  margin :: !a !(Image m) -> Image m

instance margin Span                     // Margin is the same span on all sides
instance margin (!Span, !Span)             // (h, v) Margin is h on top and bottom and v on left and right
instance margin (!Span, !Span, !Span)       // (t, h, b) Margin is t on top, v on left and right and b on bottom
instance margin (!Span, !Span, !Span, !Span) // (t, r, b, l) Margin is t on top, r on the right, b on the bottom and l on the left

normalFontDef  :: !String !Real    -> FontDef // (normalFontDef family size) sets all other fields to "normal"

empty          :: !Span !Span         -> Image m // (empty a b) is an empty image with x-span a and y-span b
text           :: !FontDef !String    -> Image m // (text font str) is an image containg str written in font
circle         :: !Span               -> Image m // (circle a) is an image of a circle with diameter a
ellipse        :: !Span !Span         -> Image m // (ellipse a b) is an image of an ellipse with x-diameter a and y-diameter b
rect           :: !Span !Span         -> Image m // (rect a b) is an image of a rectangle with x-span a and y-span b
raw            :: !Span !Span !String -> Image m

defaultMarkers :: Markers m

xline    :: !(Maybe (Markers m)) !Span              -> Image m // (xline a) is an image of a line with x-span a and y-span zero
yline    :: !(Maybe (Markers m)) !Span              -> Image m // (yline a) is an image of a line with y-span a and x-span zero
line     :: !(Maybe (Markers m)) !Slash !Span !Span -> Image m // (line a b) is an image of a line with x-span a and y-span b
polygon  :: !(Maybe (Markers m)) ![ImageOffset]     -> Image m // (polygon xs) is an image of a polygon with coordinates xs
polyline :: !(Maybe (Markers m)) ![ImageOffset]     -> Image m // (polyline xs) is an image of a polyline with coordinates xs

fit      :: !Span !Span !(Image m) -> Image m
fitx     :: !Span       !(Image m) -> Image m
fity     :: !Span       !(Image m) -> Image m

scale    :: !Real !Real !(Image m) -> Image m
scalex   :: !Real       !(Image m) -> Image m
scaley   :: !Real       !(Image m) -> Image m

rotate   :: !Angle !(Image m) -> Image m
flipx    :: !(Image m) -> Image m
flipy    :: !(Image m) -> Image m
skewx    :: !Angle !(Image m) -> Image m
skewy    :: !Angle !(Image m) -> Image m

overlay ::                            ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
beside  ::                                ![YAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
above   ::                                ![XAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid    :: !GridDimension !GridLayout ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
collage ::                                          ![ImageOffset] ![Image m] !(Host m) -> Image m

instance <  (ImageAttr m)
instance == (ImageAttr m)

class tuneImage attr :: !(Image m) !(attr m) -> Image m
(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tuneImage attr
(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tuneImage attr

tuneIf :: !Bool !(Image m) !(attr m) -> Image m | tuneImage attr

instance tuneImage StrokeAttr, StrokeWidthAttr, FillAttr, OpacityAttr,
  OnClickAttr, OnMouseDownAttr, OnMouseUpAttr, OnMouseOverAttr,
  OnMouseMoveAttr, OnMouseOutAttr, DraggableAttr, XRadiusAttr, YRadiusAttr,
  DashAttr, MaskAttr, NoAttr

class toSVGColor a :: !a -> SVGColor
instance toSVGColor String, RGB
instance zero RGB

:: RGB = { r :: !Int, g :: !Int, b :: !Int }

instance == ImageTag
instance <  ImageTag

:: *TagSource :== *[TagRef]
:: *TagRef    :== *(!ImageTag, !*ImageTag)

tag        :: !*ImageTag !(Image m)  -> Image m
tagWithSrc :: !*TagSource !(Image m) -> *(!(!Image m, !ImageTag), !*TagSource)

instance +  ImageOffset

instance == FontDef
instance < FontDef
