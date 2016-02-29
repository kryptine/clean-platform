definition module Graphics.Scalable.Internal

from Data.Maybe import :: Maybe
from Text.HTML import :: SVGColor
from Data.Set import :: Set
from Math.Geometry import :: Angle
from StdOverloaded import class zero, class +, class -, class ~, class sign, class abs, class <, class ==, class toReal, class /, class *

:: Image m
  = { content             :: !ImageContent m    // the image elements
    , mask                :: !Maybe (Image m)   // the mask image
    , attribs             :: !Set (ImageAttr m) // the image attributes
    , transform           :: ![ImageTransform]  // [t_1, ..., t_n] transforms the image as t_1 o ... o t_n
    , tags                :: !Set ImageTag      // set of tags
    , uniqId              :: !Int               // A unique identifier for this image
    // TODO Get rid of the next two fields
    , totalSpanPreTrans   :: !ImageSpan         // Total image span before transformations
    , totalSpanPostTrans  :: !ImageSpan         // Total image span after transformations
    , transformCorrection :: !ImageOffset       // Correction required after transformation
    }

:: ImageTransform
  = RotateImage !Angle
  | SkewXImage  !Angle
  | SkewYImage  !Angle
  | FitImage    !Span !Span
  | FitXImage   !Span
  | FitYImage   !Span
  | ScaleImage  !Real !Real
  | ScaleXImage !Real
  | ScaleYImage !Real
  | FlipXImage
  | FlipYImage

:: ImageContent m
  = Basic     !BasicImage !ImageSpan
  | Line      !(LineImage m)
  | Composite !(CompositeImage m)

:: LineImage m
  = { lineSpan    :: !ImageSpan
    , markers     :: !Maybe (Markers m)
    , lineContent :: !LineContent
    }

:: Markers m
  = { markerStart :: !Maybe (Image m)
    , markerMid   :: !Maybe (Image m)
    , markerEnd   :: !Maybe (Image m)
    }

:: LineContent
  = SimpleLineImage !Slash
  | PolygonImage    ![ImageOffset]
  | PolylineImage   ![ImageOffset]

:: Span
  = PxSpan     !Real       // (PxSpan a) is a pixels
  | LookupSpan !LookupSpan // (LookupSpan a) needs to be looked up after computing dimensions
  | AddSpan    !Span !Span  // (AddSpan a b) is span a + span b
  | SubSpan    !Span !Span  // (SubSpan a b) is span a - span b
  | MulSpan    !Span !Span  // (MulSpan a b) is span a * span k
  | DivSpan    !Span !Span  // (DivSpan a b) is span a / span k
  | AbsSpan    !Span       // (AbsSpan a)  is absolute value of span a
  | MinSpan    ![Span]     // (MinSpan as) is minimum span value in as
  | MaxSpan    ![Span]     // (MaxSpan as) is maximum span value in as

:: ImageSpan :== (!Span, !Span)

:: BasicImage
  = EmptyImage
  | TextImage !FontDef !String
  | CircleImage
  | RectImage
  | EllipseImage
  | RawImage !String

:: FontDef
  = { fontfamily  :: !String
    , fontysize   :: !Real
    , fontstretch :: !String
    , fontstyle   :: !String
    , fontvariant :: !String
    , fontweight  :: !String
    }

:: CompositeImage m
  = { host    :: !Host m
    , compose :: !Compose m
    }

:: LookupSpan
  = ColumnXSpan  !ImageTag !Int // (ColumnXSpan as a) is x-span of column number a in grid tagged with superset of as
  | RowYSpan     !ImageTag !Int // (RowYSpan as a) is y-span of row number a in grid tagged with superset of as
  | ImageXSpan   !ImageTag     // (ImageXSpan as) is x-span of image tagged with superset of as
  | ImageYSpan   !ImageTag     // (ImageYSpan as) is y-span of image tagged with superset of as
  | TextXSpan    !FontDef !String     // (TextXSpan a b) is width of text b written in font a

:: Compose m
  = AsGrid    !(!Int, !Int) ![[ImageOffset]] ![[ImageAlign]] ![[Image m]] // (AsGrid (noOfCols, noOfRows) alignments) composes elements in rows, using alignments per image
  | AsCollage               ![ImageOffset]                   ![Image m]   // AsCollage composes elements in freestyle, framed in optional host
  | AsOverlay               ![ImageOffset]   ![ImageAlign]   ![Image m]   // AsOverlay composes elements, framed in optional host or largest spans

:: ImageAttr m
  = ImageStrokeAttr        !(StrokeAttr      m)
  | ImageStrokeWidthAttr   !(StrokeWidthAttr m)
  | ImageXRadiusAttr       !(XRadiusAttr     m)
  | ImageYRadiusAttr       !(YRadiusAttr     m)
  | ImageStrokeOpacityAttr !(OpacityAttr     m)
  | ImageFillAttr          !(FillAttr        m)
  | ImageFillOpacityAttr   !(OpacityAttr     m)
  | ImageDashAttr          !(DashAttr        m)
  | ImageOnClickAttr       !(OnClickAttr     m)
  | ImageOnMouseDownAttr   !(OnMouseDownAttr m)
  | ImageOnMouseUpAttr     !(OnMouseUpAttr   m)
  | ImageOnMouseOverAttr   !(OnMouseOverAttr m)
  | ImageOnMouseMoveAttr   !(OnMouseMoveAttr m)
  | ImageOnMouseOutAttr    !(OnMouseOutAttr  m)
  | ImageDraggableAttr     !(DraggableAttr m)

:: StrokeAttr      m = { stroke      :: !SVGColor }
:: StrokeWidthAttr m = { strokewidth :: !Span     }
:: XRadiusAttr     m = { xradius     :: !Span     }
:: YRadiusAttr     m = { yradius     :: !Span     }
:: FillAttr        m = { fill        :: !SVGColor }
:: OpacityAttr     m = { opacity     :: !Real     }
:: OnClickAttr     m = { onclick     :: !(Int m -> m), local :: !Bool }
:: OnMouseDownAttr m = { onmousedown :: !(m -> m), local :: !Bool }
:: OnMouseUpAttr   m = { onmouseup   :: !(m -> m), local :: !Bool }
:: OnMouseOverAttr m = { onmouseover :: !(m -> m), local :: !Bool }
:: OnMouseMoveAttr m = { onmousemove :: !(m -> m), local :: !Bool }
:: OnMouseOutAttr  m = { onmouseout  :: !(m -> m), local :: !Bool }
:: DraggableAttr   m = { draggable   :: !Maybe ((Maybe (Set ImageTag)) Real Real m -> m) }
:: DashAttr        m = { dash        :: ![Int]    }
:: MaskAttr        m = { mask        :: !Image m  }
:: NoAttr          m = NoAttr

:: ImageTag
  = ImageTagUser !Int !String
  | ImageTagSystem !Int

:: XAlign
  = AtLeft
  | AtMiddleX
  | AtRight

:: YAlign
  = AtTop
  | AtMiddleY
  | AtBottom

:: ImageAlign  :== (!XAlign, !YAlign)
:: ImageOffset :== (!Span, !Span)
:: GridDimension = Rows !Int | Columns !Int
:: GridLayout  :== (!GridMajor, !GridXLayout, !GridYLayout)
:: GridMajor     = ColumnMajor | RowMajor
:: GridXLayout   = LeftToRight | RightToLeft
:: GridYLayout   = TopToBottom | BottomToTop

:: Host m :== Maybe (Image m)

:: Slash = Slash | Backslash

class (*.) infixl 7 a :: !a !n -> Span | toReal n
class (/.) infixl 7 a :: !a !n -> Span | toReal n

instance zero Span
instance +    Span
instance -    Span
instance abs  Span
instance ~    Span
instance *.   Span, Real, Int
instance *    Span
instance /.   Span, Real, Int
instance /    Span

strictTRMapRev :: !(.a -> .b) ![.a] -> [.b]
strictTRMap    :: !(.a -> .b) ![.a] -> [.b]
reverseTR      :: ![.a] -> [.a]
