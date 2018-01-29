definition module Graphics.Scalable.Internal.Image

from Data.Maybe import :: Maybe
from Data.Set import :: Set
from Data.Map import :: Map
from Data.Error import :: MaybeError (..)
from Math.Geometry import :: Angle
from StdOverloaded import class zero (..), class + (..), class -  (..), class ~ (..), class sign (..), 
                          class abs  (..), class < (..), class == (..), class toReal (..), class / (..), class * (..)
import Graphics.Scalable.Types
import Graphics.Scalable.Internal.Types

:: Image` m           :== TextSpans [ImgTagNo] -> (Img, ImgEventhandlers m, ImgTexts, ImgMasks, ImgSpans, GridSpans, ImgTags, [ImgTagNo])
:: TextSpans          :== Map FontDef (FontDescent, Map String TextSpan)  // of each font, the font descent and of each text the width
:: ImgEventhandlers m :== Map ImgTagNo [ImgEventhandler m]                // the registered event handlers of the image identified by the id (once registered, an event handler can not be overruled)
:: ImgTexts           :== Map FontDef (Set String)                        // of each font, the collection of texts
:: ImgMasks           :== Map ImgTagNo Img                                // of each mask, the mask-image (associate the id with (MaskImg id))
:: ImgSpans           :== Map ImgTagNo ImageSpan                          // of each image, its (width,height)
:: GridSpans          :== Map ImgTagNo GridSpan                           // of each grid, the spans of its columns and the spans of its rows
:: ImgTags            :== Map ImageTag ImgTagNo                           // map user-tag to system number
:: FontDescent        :== Real
:: TextSpan           :== Real
:: ImgEventhandler m
  = ImgEventhandlerOnClickAttr     !(OnClickAttr     m)
  | ImgEventhandlerOnMouseDownAttr !(OnMouseDownAttr m)
  | ImgEventhandlerOnMouseUpAttr   !(OnMouseUpAttr   m)
  | ImgEventhandlerOnMouseOverAttr !(OnMouseOverAttr m)
  | ImgEventhandlerOnMouseMoveAttr !(OnMouseMoveAttr m)
  | ImgEventhandlerOnMouseOutAttr  !(OnMouseOutAttr  m)
  | ImgEventhandlerDraggableAttr   !(DraggableAttr   m)
:: GridSpan
  = { col_spans :: ![Span]
    , row_spans :: ![Span]
    }
:: Img
  = { uniqId    :: !ImgTagNo                                              // the unique system identification within the entire image
    , host      :: !HostImg                                               // the host of this image
    , transform :: !Maybe ImgTransform                                    // the optional transform of the basic/composite image
    , overlays  :: ![Img]                                                 // the back-to-front ordering of images 'on top of' host
    , offsets   :: ![ImageOffset]                                         // the offsets matching one-by-one with .overlays
    }
:: HostImg
  = BasicHostImg !BasicImg !(Set BasicImgAttr)
  | RawHostImg   !String
  | CompositeImg !Img
:: BasicImg
  = EmptyImg
  | TextImg      !FontDef !String
  | CircleImg
  | RectImg
  | EllipseImg
  | PolylineImg  !LineMarkers ![ImageOffset]
  | PolygonImg   !LineMarkers ![ImageOffset]
:: LineMarkers
  = { lineStart :: !Maybe Img
    , lineMid   :: !Maybe Img
    , lineEnd   :: !Maybe Img
    }
:: BasicImgAttr                                                            // attributes that are applicable only on basic images
  = BasicImgStrokeAttr        !SVGColor
  | BasicImgStrokeWidthAttr   !Span
  | BasicImgXRadiusAttr       !Span
  | BasicImgYRadiusAttr       !Span
  | BasicImgStrokeOpacityAttr !Real
  | BasicImgFillOpacityAttr   !Real
  | BasicImgFillAttr          !SVGColor
  | BasicImgDashAttr          ![Int]
:: ImgTransform
  = RotateImg !Angle
  | SkewXImg  !Angle
  | SkewYImg  !Angle
  | FitImg    !Span !Span
  | FitXImg   !Span
  | FitYImg   !Span
  | ScaleImg  !Real !Real
  | FlipXImg
  | FlipYImg
  | MaskImg   !ImgTagNo                                                    // the id-img pair is stored in the ImgMasks table
:: Host` m
  = NoHost`
  | Host` (Image` m)

:: Markers` m
  = { markerStart` :: !Maybe (Image` m)
    , markerMid`   :: !Maybe (Image` m)
    , markerEnd`   :: !Maybe (Image` m)
    }

tag`        :: !*ImageTag  !(Image` m) -> Image` m
tagWithSrc` :: !*TagSource !(Image` m) -> *(!(!Image` m, !ImageTag), !*TagSource)

empty`      :: !Span !Span                  !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
text`       :: !FontDef !String             !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
circle`     :: !Span                        !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
ellipse`    :: !Span !Span                  !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
rect`       :: !Span !Span                  !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
raw`        :: !Span !Span !String          !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
polyline`   :: !(Markers` m) ![ImageOffset] !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
polygon`    :: !(Markers` m) ![ImageOffset] !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])

fit`        :: !Span !Span !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
fitx`       :: !Span       !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
fity`       :: !Span       !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
scale`      :: !Real !Real !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
rotate`     :: !Angle      !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
flipx`      ::             !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
flipy`      ::             !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
skewx`      :: !Angle      !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
skewy`      :: !Angle      !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
mask`       :: !(Image` m) !(Image` m)      !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])

grid`       :: !GridDimension !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image` m] !(Host` m)
                                            !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])
overlay`    :: ![XYAlign] ![ImageOffset] ![Image` m] !(Host` m)
                                            !TextSpans ![ImgTagNo] -> (!Img,!ImgEventhandlers m,!ImgTexts,!ImgMasks,!ImgSpans,!GridSpans,!ImgTags,![ImgTagNo])

tuneAttr`    :: !(Image` m) !BasicImgAttr -> Image` m
tuneHandler` :: !(Image` m) !(ImgEventhandler m) -> Image` m


:: SpanResolveError :== String

resolve_spans :: !ImgTags !TextSpans !Img !ImgMasks !ImgSpans !GridSpans -> MaybeError SpanResolveError (!Img,!ImgMasks,!ImgSpans,!GridSpans)

// only for testing purposes:
//dummyTextSpans :: ImgTexts -> TextSpans
