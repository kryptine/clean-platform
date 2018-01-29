implementation module Graphics.Scalable.Internal.Image

import _SystemArray
from StdBool import &&, ||
from StdFunc import o
from StdMisc import abort
from StdOrdList import minList, maxList
import StdString
from StdTuple import fst, snd
import Data.List
from Data.Set import :: Set, instance == (Set a), instance < (Set a), fold, fromList, toList, toAscList
from Data.Map import :: Map
from Data.Maybe import :: Maybe (..), fromJust, maybeToList
import Data.Error
from Data.Functor        import class Functor (..)
from Control.Applicative import class Applicative (..)
import Control.Monad
import Data.MapCollection
import qualified Data.Set as DS
import qualified Data.Map as DM
from Text.HTML import :: SVGColor (..)
import Math.Geometry
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
:: BasicImgAttr
  = BasicImgStrokeAttr        !SVGColor
  | BasicImgStrokeWidthAttr   !Span
  | BasicImgXRadiusAttr       !Span
  | BasicImgYRadiusAttr       !Span
  | BasicImgStrokeOpacityAttr !Real
  | BasicImgFillOpacityAttr   !Real
  | BasicImgFillAttr          !SVGColor
  | BasicImgDashAttr          ![Int]
:: LineMarkers
  = { lineStart   :: !Maybe Img
    , lineMid     :: !Maybe Img
    , lineEnd     :: !Maybe Img
    }
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

defaultFilledImgAttributes :: Set BasicImgAttr
defaultFilledImgAttributes
	= 'DS'.fromList [ BasicImgStrokeAttr      (toSVGColor "black")
                    , BasicImgStrokeWidthAttr (px 1.0)
                    , BasicImgFillAttr        (toSVGColor "black")
                    , BasicImgFillOpacityAttr 1.0
                    ]

defaultOutlineImgAttributes :: Set BasicImgAttr
defaultOutlineImgAttributes
	= 'DS'.fromList [ BasicImgFillAttr        (toSVGColor "none")
                    , BasicImgStrokeAttr      (toSVGColor "black")
                    , BasicImgStrokeWidthAttr (px 1.0)
                    ]

mkBasicHostImg :: !ImgTagNo !BasicImg !(Set BasicImgAttr) -> Img
mkBasicHostImg no basicImg atts = {Img | uniqId    = no
                                       , host      = BasicHostImg basicImg atts
                                       , transform = Nothing
                                       , overlays  = []
                                       , offsets   = []
                                  }

mkTransformImg :: !ImgTagNo !Img !ImgTransform -> Img
mkTransformImg no img tf = {Img | uniqId    = no
                                , host      = CompositeImg img
                                , transform = Just tf
                                , overlays  = []
                                , offsets   = []
                           }

normalizePolyPoints :: ![ImageOffset] -> [ImageOffset]
normalizePolyPoints offsets
  #! minX = minSpan (strictTRMap fst offsets)
  #! minY = minSpan (strictTRMap snd offsets)
  = strictTRMap (\(x, y) -> (x - minX, y - minY)) offsets

getLineMarkers :: !(Markers` m) !TextSpans ![ImgTagNo] -> (!LineMarkers, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
getLineMarkers {Markers` | markerStart`, markerMid`, markerEnd`} text_spans nos
  #! (startm,es1,txts1,masks1,spans1,grids1,tags1,nos) = getMarkerImg markerStart` text_spans nos
  #! (midm,  es2,txts2,masks2,spans2,grids2,tags2,nos) = getMarkerImg markerMid`   text_spans nos
  #! (endm,  es3,txts3,masks3,spans3,grids3,tags3,nos) = getMarkerImg markerEnd`   text_spans nos
  = ({LineMarkers | lineStart = startm, lineMid = midm, lineEnd = endm}
    ,mergeMapToListss [es1,   es2,   es3   ]
    ,mergeMapToSetss  [txts1, txts2, txts3]
    ,'DM'.unions      [masks1,masks2,masks3]
    ,'DM'.unions      [spans1,spans2,spans3]
    ,'DM'.unions      [grids1,grids2,grids3]
    ,'DM'.unions      [tags1, tags2, tags3 ]
    ,nos
    )
where
	getMarkerImg :: !(Maybe (Image` m)) !TextSpans ![ImgTagNo] -> (!Maybe Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
	getMarkerImg Nothing _ nos
	  = (Nothing, 'DM'.newMap, 'DM'.newMap, 'DM'.newMap, 'DM'.newMap, 'DM'.newMap, 'DM'.newMap, nos)
	getMarkerImg (Just image) text_spans nos
	  #! (marker,es,txts,masks,spans,grids,tags,nos) = image text_spans nos
	  = (Just marker,es,txts,masks,spans,grids,tags,nos)

grid_dimension :: !GridDimension !Int -> (!Int,!Int)
grid_dimension (Rows    no) no_of_elts = let no` = max 1 no in (no_of_elts / no` + sign (no_of_elts rem no`), no`)
grid_dimension (Columns no) no_of_elts = let no` = max 1 no in (no`, no_of_elts / no` + sign (no_of_elts rem no`))

grid_layout :: !(!Int,!Int) !GridLayout ![a] -> [[a]]
grid_layout (no_of_cols,no_of_rows) (major,xlayout,ylayout) cells
  #! cells = case major of
               RowMajor = chop no_of_cols cells
               column   = transpose (chop no_of_rows cells)
  = case (xlayout,ylayout) of
      (LeftToRight, TopToBottom) = cells
      (RightToLeft, TopToBottom) = strictTRMap reverseTR cells
      (LeftToRight, BottomToTop) = reverseTR cells
      (RightToLeft, BottomToTop) = strictTRMapRev reverseTR cells

empty` :: !Span !Span !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
empty` xspan yspan text_spans [no:nos]
	= (mkBasicHostImg no EmptyImg 'DS'.newSet
      ,'DM'.newMap                         // no event handlers
      ,mergeMapToSets x_txts y_txts        // text references from spans
      ,'DM'.newMap                         // no masks
      ,'DM'.singleton no (dx,dy)           // span
      ,'DM'.newMap                         // no grids
      ,'DM'.newMap                         // no tags
      ,nos                                 // remaining system tags
      )
where
	 (x_txts,xspan`) = spanImgTexts text_spans xspan
	 (y_txts,yspan`) = spanImgTexts text_spans yspan
	 dx              = maxSpan [zero,xspan`]
	 dy              = maxSpan [zero,yspan`]

text` :: !FontDef !String !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
text` font str text_spans [no:nos]
  #! font`   = {FontDef | font & fontysize = max zero font.FontDef.fontysize}
  #! (txt,w) = spanImgTexts text_spans (LookupSpan (TextXSpan font` str))
  #! h       = px font`.FontDef.fontysize
  = (mkBasicHostImg no (TextImg font` str) 'DS'.newSet
    ,'DM'.newMap                         // no event handlers
    ,txt                                 // text reference if text is not part of text_spans
    ,'DM'.newMap                         // no masks
    ,'DM'.singleton no (w,h)             // span
    ,'DM'.newMap                         // no grids
    ,'DM'.newMap                         // no tags
    ,nos                                 // remaining system tags
    )

circle` :: !Span !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
circle` diameter text_spans [no:nos]
	= (mkBasicHostImg no CircleImg defaultFilledImgAttributes
      ,'DM'.newMap                         // no event handlers
      ,txts                                // text references from diameter span
      ,'DM'.newMap                         // no masks
      ,'DM'.singleton no (d,d)             // span
      ,'DM'.newMap                         // no grids
      ,'DM'.newMap                         // no tags
      ,nos                                 // remaining system tags
      )
where
	(txts,diameter`) = spanImgTexts text_spans diameter
	d                = maxSpan [zero,diameter`]

ellipse` :: !Span !Span !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
ellipse` diax diay text_spans [no:nos]
	= (mkBasicHostImg no EllipseImg defaultFilledImgAttributes
      ,'DM'.newMap                           // no event handlers
      ,mergeMapToSets x_txts y_txts          // text references from diameter spans
      ,'DM'.newMap                           // no masks
      ,'DM'.singleton no (dx,dy)             // span
      ,'DM'.newMap                           // no grids
      ,'DM'.newMap                           // no tags
      ,nos                                   // remaining system tags
      )
where
	(x_txts,diax`) = spanImgTexts  text_spans diax
	(y_txts,diay`) = spanImgTexts  text_spans diay
	dx             = maxSpan [zero,diax`]
	dy             = maxSpan [zero,diay`]

rect` :: !Span !Span !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
rect` xspan yspan text_spans [no:nos]
	= (mkBasicHostImg no RectImg defaultFilledImgAttributes
      ,'DM'.newMap                       // no event handlers
      ,mergeMapToSets x_txts y_txts      // text references from spans
      ,'DM'.newMap                       // no masks
      ,'DM'.singleton no (dx,dy)         // span
      ,'DM'.newMap                       // no grids
      ,'DM'.newMap                       // no tags
      ,nos                               // remaining system tags
      )
where
	(x_txts,xspan`) = spanImgTexts  text_spans xspan
	(y_txts,yspan`) = spanImgTexts  text_spans yspan
	dx              = maxSpan [zero,xspan`]
	dy              = maxSpan [zero,yspan`]

raw` :: !Span !Span !String !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
raw` xspan yspan svgStr text_spans [no:nos]
	= ({Img | uniqId    = no
            , host      = RawHostImg svgStr
            , transform = Nothing
            , overlays  = []
            , offsets   = []
       }
      ,'DM'.newMap                        // no event handlers
      ,mergeMapToSets x_txts y_txts       // text references from spans
      ,'DM'.newMap                        // no masks
      ,'DM'.singleton no (dx,dy)          // span
      ,'DM'.newMap                        // no grids
      ,'DM'.newMap                        // no tags
      ,nos                                // remaining system tags
      )
where
	(x_txts,xspan`) = spanImgTexts text_spans xspan
	(y_txts,yspan`) = spanImgTexts text_spans yspan
	dx              = maxSpan [zero,xspan`]
	dy              = maxSpan [zero,yspan`]

polyline` :: !(Markers` m) ![ImageOffset] !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
polyline` markers offsets text_spans [no:nos]
| no_of_offsets < 2
    = abort ("Graphics.Scalable: polyline must be applied to at least 2 ImageOffset values instead of " +++ toString no_of_offsets)
| otherwise
	= let (markers`,es,txts,masks,spans,grids,tags,nos`) = getLineMarkers markers text_spans nos
       in (mkBasicHostImg no (PolylineImg markers` offsets``) defaultOutlineImgAttributes
          ,es
          ,mergeMapToSets span_txts txts
          ,masks
          ,'DM'.put no (dx,dy) spans
          ,grids
          ,tags
          ,nos`
          )
where
	no_of_offsets        = length offsets
	(span_txts,offsets`) = offsetsImgTexts text_spans offsets
	offsets``            = normalizePolyPoints offsets`
	dx                   = maxSpan (strictTRMap fst offsets``)
	dy                   = maxSpan (strictTRMap snd offsets``)

polygon` :: !(Markers` m) ![ImageOffset] !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
polygon` markers offsets text_spans [no:nos]
| no_of_offsets < 3
    = abort ("Graphics.Scalable: polygon must be applied to at least 3 ImageOffset values instead of " +++ toString no_of_offsets)
| otherwise
	= let (markers`,es,txts,masks,spans,grids,tags,nos`) = getLineMarkers markers text_spans nos
       in (mkBasicHostImg no (PolygonImg markers` offsets``) defaultFilledImgAttributes
          ,es
          ,mergeMapToSets span_txts txts
          ,masks
          ,'DM'.put no (dx,dy) spans
          ,grids
          ,tags
          ,nos`
          )
where
	no_of_offsets        = length offsets
	(span_txts,offsets`) = offsetsImgTexts text_spans offsets
	offsets``            = normalizePolyPoints offsets`
	dx                   = maxSpan (strictTRMap fst offsets``)
	dy                   = maxSpan (strictTRMap snd offsets``)

rotate` :: !Angle !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
rotate` a image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
	      a`                                        = normalize a
       in (mkTransformImg no img (RotateImg a`)
          ,es
          ,txts
          ,masks
          ,'DM'.put no ('DM'.find img.Img.uniqId spans) spans    // span of (rotate img) = span of img
          ,grids
          ,tags
          ,nos`
          )

flipx` :: !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
flipx` image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
       in (mkTransformImg no img FlipXImg
          ,es
          ,txts
          ,masks
          ,'DM'.put no ('DM'.find img.Img.uniqId spans) spans    // span of (flipx img) = span of img
          ,grids
          ,tags
          ,nos`
          )

flipy` :: !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
flipy` image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
       in (mkTransformImg no img FlipYImg
          ,es
          ,txts
          ,masks
          ,'DM'.put no ('DM'.find img.Img.uniqId spans) spans    // span of (flipy img) = span of img
          ,grids
          ,tags
          ,nos`
          )

fit` :: !Span !Span !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
fit` xspan yspan image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
       in (mkTransformImg no img (FitImg dx dy)
          ,es
          ,mergeMapToSetss [txts, x_txts, y_txts]
          ,masks
          ,'DM'.put no (dx,dy) spans
          ,grids
          ,tags
          ,nos`
          )
where
	(x_txts,xspan`) = spanImgTexts  text_spans xspan
	(y_txts,yspan`) = spanImgTexts  text_spans yspan
	dx              = maxSpan [zero,xspan`]
	dy              = maxSpan [zero,yspan`]

fitx` :: !Span !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
fitx` xspan image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
          (oldx,oldy)                               = 'DM'.find img.Img.uniqId spans
       in (mkTransformImg no img (FitXImg dx)
          ,es
          ,mergeMapToSets x_txts txts
          ,masks
          ,'DM'.put no (dx,oldy * (dx / oldx)) spans
          ,grids
          ,tags
          ,nos`
          )
where
	(x_txts,xspan`) = spanImgTexts text_spans xspan
	dx              = maxSpan [zero,xspan`]

fity` :: !Span !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
fity` yspan image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
          (oldx,oldy)                               = 'DM'.find img.Img.uniqId spans
       in (mkTransformImg no img (FitYImg dy)
          ,es
          ,mergeMapToSets y_txts txts
          ,masks
          ,'DM'.put no (oldx * (dy / oldy),dy) spans
          ,grids
          ,tags
          ,nos`
          )
where
	(y_txts,yspan`) = spanImgTexts text_spans yspan
	dy              = maxSpan [zero,yspan`]

scale` :: !Real !Real !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
scale` fx fy image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
          (dx,dy)                                   = 'DM'.find img.Img.uniqId spans
       in (mkTransformImg no img (ScaleImg fx` fy`)
          ,es
          ,txts
          ,masks
          ,'DM'.put no (dx *. fx`, dy *. fy`) spans
          ,grids
          ,tags
          ,nos`
          )
where
	fx` = max zero fx
	fy` = max zero fy

skewx` :: !Angle !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
skewx` a image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
	      a`                                        = normalize a
       in (mkTransformImg no img (SkewXImg a`)
          ,es
          ,txts
          ,masks
          ,'DM'.put no ('DM'.find img.Img.uniqId spans) spans
          ,grids
          ,tags
          ,nos`
          )

skewy` :: !Angle !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
skewy` a image text_spans [no:nos]
	= let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
	      a`                                        = normalize a
       in (mkTransformImg no img (SkewYImg a`)
          ,es
          ,txts
          ,masks
          ,'DM'.put no ('DM'.find img.Img.uniqId spans) spans
          ,grids
          ,tags
          ,nos`
          )

overlay` :: ![XYAlign] ![ImageOffset] ![Image` m] !(Host` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
overlay` aligns offsets images host text_spans nos
  #! l        = length images
  #! aligns`  = take l (aligns ++ repeat (AtLeft,AtTop))
  #! offsets` = take l (offsets ++ repeat (zero, zero))
  = overlay aligns` offsets` images host text_spans nos
where
	overlay :: ![XYAlign] ![ImageOffset] ![Image` m] !(Host` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
	overlay aligns offsets images NoHost` text_spans [no:nos]
	  #! (imgs,es,txts,masks,spans,grids,tags,nos`) = toImgs images text_spans nos
	  #! span_imgs                                  = ['DM'.find uniqId spans \\ {Img | uniqId} <- imgs]
	  #! span_host                                  = bounding_box_of_spans span_imgs
	  #! (offsets_txts,offsets)                     = offsetsImgTexts text_spans offsets
	  = ({Img | uniqId    = no
	          , host      = BasicHostImg EmptyImg 'DS'.newSet
	          , transform = Nothing
	          , overlays  = imgs
	          , offsets   = [ offset_within_host span_img align offset span_host
	                        \\ span_img <- span_imgs & align <- aligns & offset <- offsets
	                        ]
	     }
	    ,es
	    ,mergeMapToSets txts offsets_txts
	    ,masks
	    ,'DM'.put no span_host spans
	    ,grids
	    ,tags
	    ,nos`
	    )
	overlay aligns offsets images (Host` image) text_spans [no:nos]
	  #! (imgs,es1,txts1,masks1,spans1,grids1,tags1,nos`) = toImgs images text_spans nos
	  #! (host,es2,txts2,masks2,spans2,grids2,tags2,nos`) = image text_spans nos`
	  #! span_host                                        = 'DM'.find host.Img.uniqId spans2
	  #! (offsets_txts,offsets)                           = offsetsImgTexts text_spans offsets
	  = ({Img | uniqId    = no
	          , host      = CompositeImg host
	          , transform = Nothing
	          , overlays  = imgs
	          , offsets   = [ offset_within_host ('DM'.find uniqId spans1) align offset span_host
	                        \\ {Img | uniqId} <- imgs & align <- aligns & offset <- offsets
	                        ]
	     }
	    ,mergeMapToLists es1 es2
	    ,mergeMapToSetss [txts1,txts2,offsets_txts]
	    ,'DM'.union masks2 masks1
	    ,'DM'.put no span_host ('DM'.unions [spans1,spans2])
	    ,'DM'.union grids1 grids2
	    ,'DM'.union tags1 tags2
	    ,nos`
	    )
	
	offset_within_host :: !ImageSpan !XYAlign !ImageOffset !ImageSpan -> ImageOffset
	offset_within_host (x_img,y_img) (x_align,y_align) (x_offset,y_offset) (x_host,y_host)
		= (x_offset + x_offset_within_host x_img x_align x_host
		  ,y_offset + y_offset_within_host y_img y_align y_host
		  )
	where
		x_offset_within_host :: !Span !XAlign !Span -> Span
		x_offset_within_host x_img AtLeft    width = zero
		x_offset_within_host x_img AtMiddleX width = (width - x_img) /. 2
		x_offset_within_host x_img AtRight   width = width - x_img
		
		y_offset_within_host :: !Span !YAlign !Span -> Span
		y_offset_within_host y_img AtTop     height = zero
		y_offset_within_host y_img AtMiddleY height = (height - y_img) /. 2
		y_offset_within_host y_img AtBottom  height = height - y_img

grid` :: !GridDimension !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image` m] !(Host` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
grid` dimension layout aligns column_widths row_heights offsets images host text_spans nos
  #! l                       = length images
  #! (no_of_cols,no_of_rows) = grid_dimension dimension l
  #! no_of_cells             = no_of_cols * no_of_rows
  #! aligns`                 = take no_of_cells (aligns  ++ repeat (AtLeft,AtTop))
  #! offsets`                = take no_of_cells (offsets ++ repeat (zero,zero))
  #! images`                 = take no_of_cells (images  ++ repeat (empty` zero zero))
  = grid (no_of_cols,no_of_rows) layout aligns` column_widths row_heights offsets` images` host text_spans nos
where
	grid :: !(!Int,!Int) !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image` m] !(Host` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
	grid (no_of_cols,no_of_rows) layout aligns column_widths row_heights offsets images h text_spans [no:nos]
	  #! (imgs,es1,txts1,masks1,spans1,grids1,tags1,nos`) = toImgs images text_spans nos
	  #! (offsets_txts,offsets)                           = offsetsImgTexts text_spans offsets
	  #! (col_widths_txts,column_widths)                  = spansImgTexts   text_spans column_widths
	  #! (row_heights_txts,row_heights)                   = spansImgTexts   text_spans row_heights
	  #! imgid_span_align_offsets                         = [(uniqId,'DM'.find uniqId spans1,align,offset) \\ {Img | uniqId} <- imgs & align <- aligns & offset <- offsets]
	  #! imgid_span_align_offsets_grid                    = grid_layout (no_of_cols,no_of_rows) layout imgid_span_align_offsets
	  #! cell_spans                                       = [[span \\ (_,span,_,_) <- row] \\ row <- imgid_span_align_offsets_grid]
	  #! grid_widths                                      = constrain column_widths (cols_widths  cell_spans)
	  #! grid_heights                                     = constrain row_heights   (rows_heights cell_spans)
	  #! grid_info                                        = {col_spans = grid_widths, row_spans = grid_heights}
	  #! (grid_width,grid_height)                         = (sum grid_widths, sum grid_heights)
	  #! (host,es2,txts2,masks2,spans2,grids2,tags2,nos`) = case h of
	                                                           Host` image = image text_spans nos`
	                                                           no_host     = empty` grid_width grid_height text_spans nos`
	  #! span_host                                        = case h of
	                                                           Host` image = 'DM'.find host.Img.uniqId spans2
	                                                           no_host     = (grid_width,grid_height)
	  #! imgid_offsets                                    = offsets_within_grid grid_widths grid_heights imgid_span_align_offsets_grid
	  #! offsets                                          = associate_offset_with_img imgid_offsets imgs
	  = ({Img | uniqId    = no
	          , host      = CompositeImg host
	          , transform = Nothing
	          , overlays  = imgs
	          , offsets   = offsets
	     }
	    ,mergeMapToLists es1 es2
	    ,mergeMapToSetss [txts1,txts2,offsets_txts,col_widths_txts,row_heights_txts]
	    ,'DM'.union masks2 masks1
	    ,'DM'.put no span_host ('DM'.union spans1 spans2)
	    ,'DM'.put no grid_info ('DM'.union grids1 grids2)
	    ,'DM'.union tags1 tags2
	    ,nos`
	    )
	
	cols_widths :: ![[ImageSpan]] -> [Span]
	cols_widths cells = [maxSpan (map fst column) \\ column <- transpose cells]
	
	rows_heights :: ![[ImageSpan]] -> [Span]
	rows_heights cells = [maxSpan (map snd row) \\ row <- cells]
	
	offsets_within_grid :: ![Span] ![Span] ![[(ImgTagNo,ImageSpan,XYAlign,ImageOffset)]] -> [[(ImgTagNo,ImageOffset)]]
	offsets_within_grid grid_widths grid_heights cells
		= [ [  offset_within_grid col_offset col_width row_offset row_height cell 
		    \\ cell       <- row 
		     & col_offset <- scan (+) zero grid_widths
		     & col_width  <- grid_widths
		    ] 
		  \\ row          <- cells 
		   & row_offset   <- scan (+) zero grid_heights
		   & row_height   <- grid_heights
		  ]
	where
		offset_within_grid :: !Span !Span !Span !Span !(!ImgTagNo,!ImageSpan,!XYAlign,!ImageOffset) -> (!ImgTagNo,!ImageOffset)
		offset_within_grid column_offset column_width row_offset row_height (img_id,(img_width,img_height),(x_align,y_align),(dx,dy))
			= (img_id, (column_offset + dx + x_align_offset, row_offset + dy + y_align_offset))
		where
			x_align_offset = case x_align of
			                   AtLeft    = zero
			                   AtMiddleX = (column_width - img_width) /. 2.0
			                   AtRight   =  column_width - img_width
			y_align_offset = case y_align of
			                   AtTop     = zero
			                   AtMiddleY = (row_height - img_height) /. 2.0
			                   AtBottom  =  row_height - img_height
	
	associate_offset_with_img :: ![[(ImgTagNo,ImageOffset)]] ![Img] -> [ImageOffset]
	associate_offset_with_img cells imgs = [fromJust (lookup uniqId offsets) \\ {Img | uniqId} <- imgs]
	where
		offsets = flatten cells
	
bounding_box_of_spans :: ![(Span,Span)] -> (!Span,!Span)
bounding_box_of_spans [] = (zero,zero)
bounding_box_of_spans spans = (maxSpan widths, maxSpan heights)
where
	(widths, heights) = unzip spans

toImgs :: ![Image` m] !TextSpans ![ImgTagNo] -> (![Img], !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
toImgs images text_spans nos
	= foldr (\image (imgs,es,txts,masks,spans,grids,tags,nos)
	            -> let (img,es`,txts`,masks`,spans`,grids`,tags`,nos`) = image text_spans nos
	                in ([img:imgs],mergeMapToLists es` es,mergeMapToSets txts` txts,'DM'.union masks` masks,'DM'.union spans` spans,'DM'.union grids` grids,'DM'.union tags` tags,nos`))
	        ([],'DM'.newMap,'DM'.newMap,'DM'.newMap,'DM'.newMap,'DM'.newMap,'DM'.newMap,nos)
	        images

mask` :: !(Image` m) !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
mask` m image text_spans [no:nos]
	= let (img,es1,txts1,masks1,spans1,grids1,tags1,nos1) = image text_spans nos
          (m`, es2,txts2,masks2,spans2,grids2,tags2,nos2) = m text_spans nos1
       in (mkTransformImg no img (MaskImg m`.Img.uniqId)         // this *must* be the id of m`, because for that an svg-definition is generated as the mask-image
          ,mergeMapToLists es1 es2
          ,mergeMapToSets txts1 txts2
          ,'DM'.put m`.Img.uniqId m` ('DM'.union masks2 masks1)  // so, m` is a new mask image
          ,'DM'.put no ('DM'.find img.Img.uniqId spans1) ('DM'.union spans1 spans2)
          ,'DM'.union grids1 grids2
          ,'DM'.union tags1 tags2
          ,nos2
          )

tuneAttr` :: !(Image` m) !BasicImgAttr -> Image` m
tuneAttr` image attr = \text_spans nos -> let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
                                              (txts`,attr`)                             = imgAttrTexts text_spans attr
                                              img`                                      = {Img | img & host = add_basic_attribute attr` img.Img.host}
                                           in (img`,es,'DM'.union txts` txts,masks,spans,grids,tags,nos`)

tuneHandler` :: !(Image` m) !(ImgEventhandler m) -> Image` m
tuneHandler` image attr = \text_spans nos -> let (img,es,txts,masks,spans,grids,tags,nos`) = image text_spans nos
                                                 es`                                       = 'DM'.alter (add_new_eventhandler attr) img.Img.uniqId es
                                              in (img,es`,txts,masks,spans,grids,tags,nos`)

add_basic_attribute :: !BasicImgAttr !HostImg -> HostImg
add_basic_attribute attr (BasicHostImg img attrs) = BasicHostImg img ('DS'.insert attr attrs)
add_basic_attribute _    host = host

add_new_eventhandler :: !(ImgEventhandler m) !(Maybe [ImgEventhandler m]) -> Maybe [ImgEventhandler m]
add_new_eventhandler h Nothing
	= Just [h]
add_new_eventhandler h (Just hs)
| any (match_eventhandler h) hs = Just hs
| otherwise                     = Just [h:hs]

match_eventhandler :: !(ImgEventhandler m) !(ImgEventhandler m) -> Bool
match_eventhandler (ImgEventhandlerOnClickAttr     _) (ImgEventhandlerOnClickAttr     _) = True
match_eventhandler (ImgEventhandlerOnMouseDownAttr _) (ImgEventhandlerOnMouseDownAttr _) = True
match_eventhandler (ImgEventhandlerOnMouseUpAttr   _) (ImgEventhandlerOnMouseUpAttr   _) = True
match_eventhandler (ImgEventhandlerOnMouseOverAttr _) (ImgEventhandlerOnMouseOverAttr _) = True
match_eventhandler (ImgEventhandlerOnMouseMoveAttr _) (ImgEventhandlerOnMouseMoveAttr _) = True
match_eventhandler (ImgEventhandlerOnMouseOutAttr  _) (ImgEventhandlerOnMouseOutAttr  _) = True
match_eventhandler (ImgEventhandlerDraggableAttr   _) (ImgEventhandlerDraggableAttr   _) = True
match_eventhandler _ _ = False

ImgEventhandlerConsName :: !(ImgEventhandler m) -> String
ImgEventhandlerConsName (ImgEventhandlerOnClickAttr     _) = "ImgEventhandlerOnClickAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseDownAttr _) = "ImgEventhandlerOnMouseDownAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseUpAttr   _) = "ImgEventhandlerOnMouseUpAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseOverAttr _) = "ImgEventhandlerOnMouseOverAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseMoveAttr _) = "ImgEventhandlerOnMouseMoveAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseOutAttr  _) = "ImgEventhandlerOnMouseOutAttr"
ImgEventhandlerConsName (ImgEventhandlerDraggableAttr   _) = "ImgEventhandlerDraggableAttr"

instance <  (ImgEventhandler m) where <  a b = ImgEventhandlerConsName a <  ImgEventhandlerConsName b
instance == (ImgEventhandler m) where == a b = ImgEventhandlerConsName a == ImgEventhandlerConsName b

ImgAttrConsName :: !BasicImgAttr -> String
ImgAttrConsName (BasicImgStrokeAttr        _) = "BasicImgStrokeAttr"
ImgAttrConsName (BasicImgStrokeWidthAttr   _) = "BasicImgStrokeWidthAttr"
ImgAttrConsName (BasicImgXRadiusAttr       _) = "BasicImgXRadiusAttr"
ImgAttrConsName (BasicImgYRadiusAttr       _) = "BasicImgYRadiusAttr"
ImgAttrConsName (BasicImgStrokeOpacityAttr _) = "BasicImgStrokeOpacityAttr"
ImgAttrConsName (BasicImgFillOpacityAttr   _) = "BasicImgFillOpacityAttr"
ImgAttrConsName (BasicImgFillAttr          _) = "BasicImgFillAttr"
ImgAttrConsName (BasicImgDashAttr          _) = "BasicImgDashAttr"

instance <  BasicImgAttr where <  a b = ImgAttrConsName a <  ImgAttrConsName b
instance == BasicImgAttr where == a b = ImgAttrConsName a == ImgAttrConsName b

tag` :: !*ImageTag !(Image` m) -> Image` m
tag` (ImageTagUser no label) image = tag (ImageTagUser no label) image
where
	tag :: !ImageTag !(Image` m) !TextSpans ![ImgTagNo] -> (!Img, !ImgEventhandlers m, !ImgTexts, !ImgMasks, !ImgSpans, !GridSpans, !ImgTags, ![ImgTagNo])
	tag t image txt_spans nos
	  #! (img,es,txts,masks,spans,grids,tags,nos) = image txt_spans nos
	  =  (img,es,txts,masks,spans,grids,'DM'.put t img.Img.uniqId tags,nos)
tag` (ImageTagSystem no) _
	= abort "Graphics.Scalable.Image: tag applied to unexpected ImageTag"

tagWithSrc` :: !*TagSource !(Image` m) -> *(!(!Image` m, !ImageTag), !*TagSource)
tagWithSrc` [(nut, t) : tsrc] img
  = ((tag` t img, nut), tsrc)


/** chop n xs = xss:
      @xss consists of the subsequent sub-lists of @xs of length @n.
      The length of the last element of @xss can be less than @n.
*/
chop :: !Int ![a] -> [[a]]
chop n [] = []
chop n xs
  #! (firstN, withoutN) = splitAt n xs
  = [firstN : chop n withoutN]

/** constrain as bs = cs:
      replace the first elements of @bs by @as.
*/
constrain :: ![a] ![a] -> [a]
constrain [a:as] [_:bs] = [a:constrain as bs]
constrain _      bs     = bs

spanImgTexts :: !TextSpans !Span -> (!ImgTexts,!Span)
spanImgTexts text_spans span
  = case span of
      LookupSpan (TextXSpan font str) = case lookupTextSpan font str text_spans of
                                          Just w  = ('DM'.newMap, px w)
                                          no_info = ('DM'.singleton font ('DS'.singleton str),span)
      AddSpan sp1 sp2                 = spanImgTexts` text_spans (foldl1 (+)) [sp1,sp2]
      SubSpan sp1 sp2                 = spanImgTexts` text_spans (foldl1 (-)) [sp1,sp2]
      MulSpan sp1 sp2                 = spanImgTexts` text_spans (foldl1 (*)) [sp1,sp2]
      DivSpan sp1 sp2                 = spanImgTexts` text_spans (foldl1 (/)) [sp1,sp2]
      AbsSpan sp                      = spanImgTexts` text_spans (abs o hd)   [sp]
      MinSpan sps                     = spanImgTexts` text_spans minSpan      sps
      MaxSpan sps                     = spanImgTexts` text_spans maxSpan      sps
      span                            = ('DM'.newMap,span)
where
	spanImgTexts` :: !TextSpans !([Span] -> Span) ![Span] -> (!ImgTexts,!Span)
	spanImgTexts` text_spans combine spans
		= (mergeMapToSetss texts, combine spans`)
	where
		(texts,spans`) = unzip (map (spanImgTexts text_spans) spans)
	
spansImgTexts :: !TextSpans ![Span] -> (!ImgTexts,![Span])
spansImgTexts text_spans spans
	= (mergeMapToSetss textss,spans`)
where
	(textss,spans`) = unzip (map (spanImgTexts text_spans) spans)

offsetsImgTexts :: !TextSpans ![ImageOffset] -> (!ImgTexts,![ImageOffset])
offsetsImgTexts text_spans offsets
	= (mergeMapToSetss imgtxts`, [(dx,dy) \\ [dx,dy:_] <- chop 2 offsets`])
where
	(imgtxts`,offsets`) = unzip (strictTRMap (spanImgTexts text_spans) (flatten (strictTRMap (\(a,b) -> [a,b]) offsets)))

imgAttrTexts :: !TextSpans !BasicImgAttr -> (!ImgTexts,!BasicImgAttr)
imgAttrTexts text_spans (BasicImgStrokeWidthAttr   span) = let (texts,span`) = spanImgTexts text_spans span in (texts,BasicImgStrokeWidthAttr span`)
imgAttrTexts text_spans (BasicImgXRadiusAttr       span) = let (texts,span`) = spanImgTexts text_spans span in (texts,BasicImgXRadiusAttr span`)
imgAttrTexts text_spans (BasicImgYRadiusAttr       span) = let (texts,span`) = spanImgTexts text_spans span in (texts,BasicImgYRadiusAttr span`)
imgAttrTexts _          attr                             = ('DM'.newMap, attr)

lookupTextSpan :: !FontDef !String !TextSpans -> Maybe Real
lookupTextSpan font str text_spans
	= case 'DM'.get font text_spans of
	    Just (_,ws) = 'DM'.get str ws
	    nothing     = Nothing


liftMaybe :: (a -> m b) (Maybe a) -> m (Maybe b) | Monad m
liftMaybe g Nothing  = pure Nothing
liftMaybe g (Just a) = g a >>= \b -> pure (Just b)

:: SpanResolveError :== String

resolve_spans :: !ImgTags !TextSpans !Img !ImgMasks !ImgSpans !GridSpans -> MaybeError SpanResolveError (!Img,!ImgMasks,!ImgSpans,!GridSpans)
resolve_spans user_tags text_spans img masks spans grids
  = resolveImgSpans  user_tags text_spans spans  grids        >>= \spans` -> 
    resolveGridSpans user_tags text_spans spans` grids        >>= \grids` -> 
    resolveImgMasks  user_tags text_spans spans` grids` masks >>= \masks` ->
    resolveImg       user_tags text_spans spans` grids` img   >>= \img`   ->
    pure (img`,masks`,spans`,grids`)

resolveImgMasks :: !ImgTags !TextSpans !ImgSpans !GridSpans !ImgMasks -> MaybeError SpanResolveError ImgMasks
resolveImgMasks user_tags text_spans spans grids masks
  = mapM (resolveImg user_tags text_spans spans grids) imgs >>= \imgs` -> pure ('DM'.fromList (zip2 img_nos imgs`))
where
	(img_nos,imgs) = unzip ('DM'.toList masks)

resolveImgSpans :: !ImgTags !TextSpans !ImgSpans !GridSpans -> MaybeError SpanResolveError ImgSpans
resolveImgSpans user_tags text_spans spans grids
  = mapM (resolve_span user_tags text_spans spans grids) (flatten [[w,h] \\ (w,h) <- img_spans]) >>= \img_spans` ->
    pure ('DM'.fromList [(img_no,(PxSpan w`,PxSpan h`)) \\ img_no <- img_nos & [w`,h`:_] <- chop 2 img_spans`])
where
	(img_nos,img_spans) = unzip ('DM'.toList spans)

resolveGridSpans :: !ImgTags !TextSpans !ImgSpans !GridSpans -> MaybeError SpanResolveError GridSpans
resolveGridSpans user_tags text_spans spans grids
  = mapM (resolveGridSpan user_tags text_spans spans grids) grid_spans >>= \grid_spans` ->
    pure ('DM'.fromList [(grid_no,grid_span`) \\ grid_no <- grid_nos & grid_span` <- grid_spans`])
where
	(grid_nos,grid_spans) = unzip ('DM'.toList grids)
	
	resolveGridSpan :: !ImgTags !TextSpans !ImgSpans !GridSpans !GridSpan -> MaybeError SpanResolveError GridSpan
	resolveGridSpan user_tags text_spans spans grids {GridSpan | col_spans,row_spans}
	  = mapM (resolve_span user_tags text_spans spans grids) col_spans >>= \col_spans` ->
	    mapM (resolve_span user_tags text_spans spans grids) row_spans >>= \row_spans` ->
	    pure {GridSpan | col_spans = map PxSpan col_spans`, row_spans = map PxSpan row_spans`}

resolveImg :: !ImgTags !TextSpans !ImgSpans !GridSpans !Img -> MaybeError SpanResolveError Img
resolveImg user_tags text_spans spans grids img=:{Img | host,overlays,offsets,transform}
  =            resolveHostImg      user_tags text_spans spans grids  host      >>= \host`      -> 
    mapM      (resolveImg          user_tags text_spans spans grids) overlays  >>= \overlays`  -> 
    mapM      (resolveImageOffset  user_tags text_spans spans grids) offsets   >>= \offsets`   ->
    liftMaybe (resolveImgTransform user_tags text_spans spans grids) transform >>= \transform` ->
    pure {Img | img & host = host`, overlays = overlays`, offsets = offsets`, transform = transform`}
where
	resolveHostImg :: !ImgTags !TextSpans !ImgSpans !GridSpans !HostImg -> MaybeError SpanResolveError HostImg
	resolveHostImg user_tags text_spans spans grids (BasicHostImg img attrs)
	  = resolveBasicImg user_tags text_spans spans grids img   >>= \img`   ->
	    resolveImgAttrs user_tags text_spans spans grids attrs >>= \attrs` -> pure (BasicHostImg img` attrs`)
	resolveHostImg user_tags text_spans spans grids (CompositeImg img)
	  = resolveImg user_tags text_spans spans grids img >>= \img` -> pure (CompositeImg img`)
	resolveHostImg user_tags text_spans spans grids host
	  = pure host
	
	resolveImgAttrs :: !ImgTags !TextSpans !ImgSpans !GridSpans !(Set BasicImgAttr) -> MaybeError SpanResolveError (Set BasicImgAttr)
	resolveImgAttrs user_tags text_spans spans grids attrs
/*	  = mapM (resolveImgAttr user_tags text_spans spans grids) (toList attrs) >>= \attrs` -> // USING (toList attrs) INSTEAD OF (fold (\a as -> [a:as]) [] attrs) CRASHES THE COMPILER: Run Time Error: index out of range
	    pure (fromList attrs`)*/
	  = mapM (resolveImgAttr user_tags text_spans spans grids) (fold (\a as -> [a:as]) [] attrs) >>= \attrs` -> pure (fromList attrs`)
	
	resolveImgAttr :: !ImgTags !TextSpans !ImgSpans !GridSpans !BasicImgAttr -> MaybeError SpanResolveError BasicImgAttr
	resolveImgAttr user_tags text_spans spans grids (BasicImgStrokeWidthAttr span)
	  = resolve_span user_tags text_spans spans grids span >>= \r -> pure (BasicImgStrokeWidthAttr (px r))
	resolveImgAttr user_tags text_spans spans grids (BasicImgXRadiusAttr span)
	  = resolve_span user_tags text_spans spans grids span >>= \r -> pure (BasicImgXRadiusAttr (px r))
	resolveImgAttr user_tags text_spans spans grids (BasicImgYRadiusAttr span)
	  = resolve_span user_tags text_spans spans grids span >>= \r -> pure (BasicImgYRadiusAttr (px r))
	resolveImgAttr user_tags text_spans spans grids attr = pure attr
	
	resolveImgTransform :: !ImgTags !TextSpans !ImgSpans !GridSpans !ImgTransform -> MaybeError SpanResolveError ImgTransform
	resolveImgTransform user_tags text_spans spans grids (FitImg w h)
	  = resolve_span user_tags text_spans spans grids w >>= \w` -> 
	    resolve_span user_tags text_spans spans grids h >>= \h` -> pure (FitImg (px w`) (px h`))
	resolveImgTransform user_tags text_spans spans grids (FitXImg w)
	  = resolve_span user_tags text_spans spans grids w >>= \w` -> pure (FitXImg (px w`))
	resolveImgTransform user_tags text_spans spans grids (FitYImg h)
	  = resolve_span user_tags text_spans spans grids h >>= \h` -> pure (FitYImg (px h`))
	resolveImgTransform user_tags text_spans spans grids transform
	  = pure transform
	
	resolveImageOffset :: !ImgTags !TextSpans !ImgSpans !GridSpans !ImageOffset -> MaybeError SpanResolveError ImageOffset
	resolveImageOffset user_tags text_spans spans grids (w,h)
	  = resolve_span user_tags text_spans spans grids w >>= \w` ->
	    resolve_span user_tags text_spans spans grids h >>= \h` -> pure (PxSpan w`, PxSpan h`)
	
	resolveBasicImg :: !ImgTags !TextSpans !ImgSpans !GridSpans !BasicImg -> MaybeError SpanResolveError BasicImg
	resolveBasicImg user_tags text_spans spans grids (PolylineImg markers offsets)
	  =       resolveLineMarkers user_tags text_spans spans grids  markers >>= \markers` ->
	    mapM (resolveImageOffset user_tags text_spans spans grids) offsets >>= \offsets` -> pure (PolylineImg markers` offsets`)
	resolveBasicImg user_tags text_spans spans grids (PolygonImg markers offsets)
	  =       resolveLineMarkers user_tags text_spans spans grids  markers >>= \markers` ->
	    mapM (resolveImageOffset user_tags text_spans spans grids) offsets >>= \offsets` -> pure (PolylineImg markers` offsets`)
	resolveBasicImg user_tags text_spans spans grids img
	  = pure img
	
	resolveLineMarkers :: !ImgTags !TextSpans !ImgSpans !GridSpans !LineMarkers -> MaybeError SpanResolveError LineMarkers
	resolveLineMarkers user_tags text_spans spans grids {LineMarkers | lineStart, lineMid, lineEnd}
	  = liftMaybe (resolveImg user_tags text_spans spans grids) lineStart >>= \lineStart` -> 
	    liftMaybe (resolveImg user_tags text_spans spans grids) lineMid   >>= \lineMid`   -> 
	    liftMaybe (resolveImg user_tags text_spans spans grids) lineEnd   >>= \lineEnd`   -> 
	    pure {LineMarkers | lineStart = lineStart`, lineMid = lineMid`, lineEnd = lineEnd`}

resolve_span :: !ImgTags !TextSpans !ImgSpans !GridSpans !Span -> MaybeError SpanResolveError Real
resolve_span user_tags text_spans spans grids span
  = resolve 'DS'.newSet user_tags text_spans spans grids span
where
	user_error f msg = Error (       "Error in " +++ f +++ ": " +++ msg)
	sys_error  f msg = Error ("System error in " +++ f +++ ": " +++ msg)
	
	resolve :: !(Set ImgTagNo) !ImgTags !TextSpans !ImgSpans !GridSpans !Span -> MaybeError SpanResolveError Real
	resolve visited user_tags text_spans spans grids (PxSpan r)
	  = pure r
	resolve visited user_tags text_spans spans grids (AddSpan a b)
	  = resolve visited user_tags text_spans spans grids a >>= \ar -> 
	    resolve visited user_tags text_spans spans grids b >>= \br -> pure (ar + br)
	resolve visited user_tags text_spans spans grids (SubSpan a b)
	  = resolve visited user_tags text_spans spans grids a >>= \ar -> 
	    resolve visited user_tags text_spans spans grids b >>= \br -> pure (ar - br)
	resolve visited user_tags text_spans spans grids (MulSpan a b)
	  = resolve visited user_tags text_spans spans grids a >>= \ar -> 
	    resolve visited user_tags text_spans spans grids b >>= \br -> pure (ar * br)
	resolve visited user_tags text_spans spans grids (DivSpan a b)
	  = resolve visited user_tags text_spans spans grids b >>= \r ->
	    if (r == 0.0) (user_error "(/.)" "division by zero")
	                  (resolve visited user_tags text_spans spans grids a >>= \x -> pure (x/r))
	resolve visited user_tags text_spans spans grids (AbsSpan a)
	  = fmap abs (resolve visited user_tags text_spans spans grids a)
	resolve visited user_tags text_spans spans grids (MinSpan as)
	  = mapM (resolve visited user_tags text_spans spans grids) as >>= \rs -> pure (minList rs)
	resolve visited user_tags text_spans spans grids (MaxSpan as)
	  = mapM (resolve visited user_tags text_spans spans grids) as >>= \rs -> pure (maxList rs)
	resolve visited user_tags text_spans spans grids (LookupSpan l)
	  = case l of
	      ColumnXSpan user_tag column_no = case user_tag of
	                                         ImageTagUser no label
	                                                           = case 'DM'.get (ImageTagUser no label)/*no*//*(no,label)*/ user_tags of
	                                                               Nothing  = user_error "columnspan" "unassigned ImageTag"
	                                                               Just no` = resolve_from_grid_span "ImageTag" get_col_spans column_no (user_error "columnspan") no` visited user_tags text_spans spans grids
	                                         ImageTagSystem no = resolve_from_grid_span "system tag" get_col_spans column_no (sys_error "columnspan") no visited user_tags text_spans spans grids
	      RowYSpan    user_tag row_no    = case user_tag of
	                                         ImageTagUser no label
	                                                           = case 'DM'.get (ImageTagUser no label)/*no*//*(no,label)*/ user_tags of
	                                                               Nothing  = user_error "rowspan" "unassigned ImageTag"
	                                                               Just no` = resolve_from_grid_span "ImageTag" get_row_spans row_no (user_error "rowspan") no` visited user_tags text_spans spans grids
	                                         ImageTagSystem no = resolve_from_grid_span "system tag" get_row_spans row_no (sys_error "rowspan") no visited user_tags text_spans spans grids
	      ImageXSpan  user_tag           = case user_tag of
	                                         ImageTagUser no label
	                                                           = case 'DM'.get (ImageTagUser no label)/*no*//*(no,label)*/ user_tags of
	                                                               Nothing  = user_error "imagexspan" "unassigned ImageTag"
	                                                               Just no` = resolve_from_image_span "ImageTag" fst (user_error "imagexspan") no` visited user_tags text_spans spans grids
	                                         ImageTagSystem no = resolve_from_image_span "system tag" fst (sys_error "imagexspan") no visited user_tags text_spans spans grids
	      ImageYSpan  user_tag           = case user_tag of
	                                         ImageTagUser no label
	                                                           = case 'DM'.get (ImageTagUser no label)/*no*//*(no,label)*/ user_tags of
	                                                               Nothing  = user_error "imageyspan" "unassigned ImageTag"
	                                                               Just no` = resolve_from_image_span "ImageTag" snd (user_error "imageyspan") no` visited user_tags text_spans spans grids
	                                         ImageTagSystem no = resolve_from_image_span "system tag" snd (sys_error "imageyspan") no visited user_tags text_spans spans grids
	      TextXSpan   font txt           = case 'DM'.get font text_spans of
	                                         Nothing           = sys_error "textxspan" ("missing FontDef entry (" +++ toString font +++ ")")
	                                         Just (_,ws)       = case 'DM'.get txt ws of
	                                                               Nothing = sys_error "textxspan" ("missing text entry \"" +++ txt +++ "\"")
	                                                               Just w  = pure w
	where		
		resolve_from_grid_span :: !String !(GridSpan -> [Span]) !Int !(String -> MaybeError SpanResolveError Real) !Int !(Set ImgTagNo) !ImgTags !TextSpans !ImgSpans !GridSpans -> MaybeError SpanResolveError Real
		resolve_from_grid_span tag_type selector elem_no error no visited user_tags text_spans spans grids
			= case 'DM'.get no grids of
	            Nothing = error (tag_type +++ " does not refer to grid")
	            Just grid = let dim_spans = selector grid
	                         in if (elem_no < 0 || elem_no >= length dim_spans)
	                               (error ("incorrect number (" +++ toString elem_no +++ ")"))
	                               (if ('DS'.member no visited)
	                                   (error "cyclic dependency of system tags")
	                                   (resolve ('DS'.insert no visited) user_tags text_spans spans grids (dim_spans !! elem_no))
	                               )
		
		resolve_from_image_span :: !String !((Span,Span) -> Span) !(String -> MaybeError SpanResolveError Real) !Int !(Set ImgTagNo) !ImgTags !TextSpans !ImgSpans !GridSpans -> MaybeError SpanResolveError Real
		resolve_from_image_span tag_type selector error no visited user_tags text_spans spans grids
	        = case 'DM'.get no spans of
	            Nothing    = error (tag_type +++ " is not associated with an image")
	            Just span  = if ('DS'.member no visited)
	                            (error ("cyclic dependency of " +++ tag_type +++ "s"))
	                            (resolve ('DS'.insert no visited) user_tags text_spans spans grids (selector span))

get_col_spans :: !GridSpan -> [Span]
get_col_spans {GridSpan | col_spans} = col_spans

get_row_spans :: !GridSpan -> [Span]
get_row_spans {GridSpan | row_spans} = row_spans

/* only for testing purposes:
dummyTextSpans :: ImgTexts -> TextSpans
dummyTextSpans txts
	= 'DM'.fromList 
		[ ( font
		  , ( font.FontDef.fontysize * 0.25
		    , 'DM'.fromList 
		          [  (line,10.0 * toReal (size line)) 
		          \\ line <- 'DS'.toList lines
		          ]
		    )
		  ) 
		\\ (font,lines) <- 'DM'.toList txts
		]
*/