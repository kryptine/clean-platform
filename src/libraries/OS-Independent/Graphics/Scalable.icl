implementation module Graphics.Scalable

import Graphics.Scalable.Internal
from StdMisc import abort
from StdFunc import flip
from StdTuple import fst, snd
from StdOrdList import minList, maxList
from StdOverloaded import class toReal
import Data.List
import Data.Maybe
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
from StdBool import &&, ||
import qualified Data.Set as DS
import Text.HTML
from Data.Functor import class Functor (..)
import Math.Geometry

isPxSpan :: !Span -> Bool
isPxSpan (PxSpan _) = True
isPxSpan _          = False

px :: !Real -> Span
px a = PxSpan a

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

mkImage :: !(ImageContent m) -> Image m
mkImage cnt =
  { content             = cnt
  , mask                = Nothing
  , attribs             = 'DS'.newSet
  , transform           = []
  , tags                = 'DS'.newSet
  , uniqId              = -1
  , totalSpanPreTrans   = (px 0.0, px 0.0)
  , totalSpanPostTrans  = (px 0.0, px 0.0)
  , transformCorrection = (px 0.0, px 0.0)
  }

class margin a where
  margin :: !a !(Image m) -> Image m

instance margin Span where
  margin sp im = margin (sp, sp, sp, sp) im

instance margin (!Span, !Span) where
  margin (sp1, sp2) im = margin (sp1, sp2, sp1, sp2) im

instance margin (!Span, !Span, !Span) where
  margin (sp1, sp2, sp3) im = margin (sp1, sp2, sp3, sp2) im

instance margin (!Span, !Span, !Span, !Span) where
  margin (sp1, sp2, sp3, sp4) im = above (repeat AtMiddleX) []
                                     [ empty zero sp1
                                     , beside (repeat AtMiddleY) [] [empty sp4 zero, im, empty sp2 zero] Nothing
                                     , empty zero sp3
                                     ] Nothing

empty :: !Span !Span -> Image m
empty xspan yspan = mkImage (Basic EmptyImage (maxSpan [zero, xspan], maxSpan [zero, yspan]))

text :: !FontDef !String -> Image m
text font str = mkImage (Basic (TextImage font str) (textxspan font str, px font.FontDef.fontysize))

circle :: !Span -> Image m
circle diameter
  #! d = maxSpan [zero, diameter]
  = { mkImage (Basic CircleImage (d, d))
    & attribs = 'DS'.fromList [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              , ImageFillAttr        {fill        = toSVGColor "black"}
                              , ImageFillOpacityAttr {opacity     = 1.0}
                              ]
    }

ellipse :: !Span !Span -> Image m
ellipse diax diay
  = { mkImage (Basic EllipseImage (maxSpan [zero, diax], maxSpan [zero, diay]))
    & attribs = 'DS'.fromList [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              , ImageFillAttr        {fill        = toSVGColor "black"}
                              , ImageFillOpacityAttr {opacity     = 1.0}
                              ]
    }

rect :: !Span !Span -> Image m
rect xspan yspan
  = { mkImage (Basic RectImage (maxSpan [zero, xspan], maxSpan [zero, yspan]))
    & attribs = 'DS'.fromList [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              , ImageFillAttr        {fill        = toSVGColor "black"}
                              , ImageFillOpacityAttr {opacity     = 1.0}
                              ]
    }

defaultMarkers :: Markers m
defaultMarkers
  = { Markers
    | markerStart = Nothing
    , markerMid   = Nothing
    , markerEnd   = Nothing
    }

xline :: !(Maybe (Markers m)) !Span -> Image m
xline markers xspan = line markers Slash xspan zero

yline :: !(Maybe (Markers m)) !Span -> Image m
yline markers yspan = line markers Slash zero yspan

line :: !(Maybe (Markers m)) !Slash !Span !Span -> Image m
line markers slash xspan yspan
  = { mkImage (Line { lineSpan    = (abs xspan, abs yspan)
                    , markers     = markers
                    , lineContent = SimpleLineImage slash
                    })
    & attribs = 'DS'.fromList [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              , ImageFillAttr        {fill        = toSVGColor "black"}
                              , ImageFillOpacityAttr {opacity     = 1.0}
                              ]
    }

polygon :: !(Maybe (Markers m)) ![ImageOffset] -> Image m
polygon markers offsets
  #! offsets = normalizePolyPoints offsets
  = mkImage (Line { lineSpan    = (maxSpan (strictTRMap fst offsets), maxSpan (strictTRMap snd offsets))
                  , markers     = markers
                  , lineContent = PolygonImage offsets
                  })

polyline :: !(Maybe (Markers m)) ![ImageOffset] -> Image m
polyline markers offsets
  #! offsets = normalizePolyPoints offsets
  = { mkImage (Line { lineSpan    = (maxSpan (strictTRMap fst offsets), maxSpan (strictTRMap snd offsets))
                    , markers     = markers
                    , lineContent = PolylineImage offsets
                    })
    & attribs = 'DS'.fromList [ ImageFillAttr        {fill        = toSVGColor "none"}
                              , ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              ]
    }

normalizePolyPoints :: ![ImageOffset] -> [ImageOffset]
normalizePolyPoints offsets
  #! minX = minSpan (strictTRMap fst offsets)
  #! minY = minSpan (strictTRMap snd offsets)
  = strictTRMap (\(x, y) -> (x - minX, y - minY)) offsets

rotate :: !Angle !(Image m) -> Image m
rotate a image=:{Image | transform = ts}
  #! a` = normalize a
  | a` == deg 0.0 = image
  | otherwise
      #! ts` = case ts of
                 [RotateImage angle : ts]
                   # a` = normalize (deg (toDeg angle + toDeg a))
                   = if (a` == deg 0.0) ts [RotateImage a` : ts]
                 ts
                   = [RotateImage a` : ts]
      = {Image | image & transform = ts`}

flipx :: !(Image m) -> Image m
flipx image = { Image | image & transform = [FlipXImage : image.transform] }

flipy :: !(Image m) -> Image m
flipy image = { Image | image & transform = [FlipYImage : image.transform] }

fit :: !Span !Span !(Image m) -> Image m
fit xspan yspan image=:{Image | transform = ts}
  #! xspan` = maxSpan [zero, xspan]
  #! yspan` = maxSpan [zero, yspan]
  #! ts`    = case ts of
                [FitImage _ _ : ts] = [FitImage xspan` yspan` : ts]
                ts                  = [FitImage xspan` yspan` : ts]
  = {Image | image & transform = ts`}

fitx :: !Span !(Image m) -> Image m
fitx xspan image=:{Image | transform = ts}
  #! xspan` = maxSpan [zero, xspan]
  #! ts`    = case ts of
                [FitXImage _ : ts] = [FitXImage xspan` : ts]
                [FitYImage _ : ts] = [FitXImage xspan` : ts]
                ts                 = [FitXImage xspan` : ts]
  = {Image | image & transform = ts`}

fity :: !Span !(Image m) -> Image m
fity yspan image=:{Image | transform = ts}
  #! yspan` = maxSpan [zero, yspan]
  #! ts`    = case ts of
                [FitXImage _ : ts] = [FitYImage yspan` : ts]
                [FitYImage _ : ts] = [FitYImage yspan` : ts]
                ts                 = [FitYImage yspan` : ts]
  = {Image | image & transform = ts`}

scale :: !Real !Real !(Image m) -> Image m
scale xspan yspan image=:{Image | transform = ts}
  #! xspan` = max zero xspan
  #! yspan` = max zero yspan
  #! ts`    = case ts of
                [ScaleImage _ _ : ts] = [ScaleImage xspan` yspan` : ts]
                ts                  = [ScaleImage xspan` yspan` : ts]
  = {Image | image & transform = ts`}

scalex :: !Real !(Image m) -> Image m
scalex xspan image=:{Image | transform = ts}
  #! xspan` = max zero xspan
  #! ts`    = case ts of
                [ScaleXImage _ : ts] = [ScaleXImage xspan` : ts]
                [ScaleYImage _ : ts] = [ScaleXImage xspan` : ts]
                ts                   = [ScaleXImage xspan` : ts]
  = {Image | image & transform = ts`}

scaley :: !Real !(Image m) -> Image m
scaley yspan image=:{Image | transform = ts}
  #! yspan` = max zero yspan
  #! ts`    = case ts of
                [ScaleXImage _ : ts] = [ScaleYImage yspan` : ts]
                [ScaleYImage _ : ts] = [ScaleYImage yspan` : ts]
                ts                   = [ScaleYImage yspan` : ts]
  = {Image | image & transform = ts`}


skewx :: !Angle !(Image m) -> Image m
skewx xskew image=:{Image | transform = ts}
  #! xskew` = normalize xskew
  | toDeg xskew` == 0.0 = image
  | otherwise
      #! ts` = case ts of
                 [SkewXImage a : ts]
                   #! a` = normalize (deg (toDeg a + toDeg xskew))
                   = if (toDeg a` == 0.0) ts [SkewXImage a` : ts]
                 ts
                   = [SkewXImage xskew` : ts]

      = {Image | image & transform = ts`}

skewy :: !Angle !(Image m) -> Image m
skewy yskew image=:{Image | transform = ts}
  #! yskew` = normalize yskew
  | toDeg yskew` == 0.0 = image
  | otherwise
      #! ts` = case ts of
                 [SkewYImage a : ts]
                   # a` = normalize (deg (toDeg a + toDeg yskew))
                   = if (toDeg a` == 0.0) ts [SkewYImage a` : ts]
                 ts
                   = [SkewYImage yskew` : ts]
      = {Image | image & transform = ts`}

overlay :: ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
overlay _      _       []   (Just img) = img
overlay _      _       []   _          = empty (px 0.0) (px 0.0)
overlay aligns offsets imgs host
  #! l        = length imgs
  #! offsets` = take l (offsets ++ repeat (zero, zero))
  #! aligns`  = take l (aligns ++ repeat (AtLeft, AtTop))
  = mkImage (Composite { host    = host
                       , compose = AsOverlay offsets` aligns` imgs
                       })

beside :: ![YAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
beside ylayouts offsets imgs host
  #! l = length imgs
  = grid (Rows 1) (RowMajor, LeftToRight, TopToBottom) (take l [(AtLeft, ylayout) \\ ylayout <- ylayouts]) (take l offsets) imgs host

above :: ![XAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
above xlayouts offsets imgs host
  #! l = length imgs
  = grid (Columns 1) (ColumnMajor, LeftToRight, TopToBottom) (take l [(xlayout, AtTop) \\ xlayout <- xlayouts]) (take l offsets) imgs host

grid :: !GridDimension !GridLayout ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid _ _ _ _ [] (Just img) = img
grid _ _ _ _ [] _          = empty (px 0.0) (px 0.0)
grid dimension (major,xlayout,ylayout) aligns offsets imgs host
  #! noOfImgs        = length imgs
  #! (cols, rows)    = case dimension of
                         Rows no
                           #! no` = max 1 no
                           = (noOfImgs / no` + sign (noOfImgs rem no`), no`)
                         Columns no
                           #! no` = max 1 no
                           = (no`, noOfImgs / no` + sign (noOfImgs rem no`))
  #! numCells        = cols * rows
  #! imgsComplete    = take numCells (imgs ++ repeat (empty (px 0.0) (px 0.0)))
  #! alignsComplete  = take numCells (aligns ++ repeat (AtLeft, AtTop))
  #! offsetsComplete = take numCells (offsets ++ repeat (zero, zero))
  #! (  imgs`
      , aligns`
      , offsets`)    = if (isRowMajor major)
                         ( chop cols imgsComplete
                         , chop cols alignsComplete
                         , chop cols offsetsComplete )
                         ( transpose (chop rows imgsComplete)
                         , transpose (chop rows alignsComplete)
                         , transpose (chop rows offsetsComplete) )
  = mkImage (Composite { host    = host
                       , compose = AsGrid (cols, rows) (arrangeLayout (xlayout,ylayout) offsets`)
                                                       (arrangeLayout (xlayout,ylayout) aligns`)
                                                       (arrangeLayout (xlayout,ylayout) imgs`)
                       })
  where
  isRowMajor :: !GridMajor -> Bool
  isRowMajor RowMajor = True
  isRowMajor _        = False

  arrangeLayout :: !(GridXLayout,!GridYLayout) ![[a]] -> [[a]]
  arrangeLayout (LeftToRight, TopToBottom) xs = xs
  arrangeLayout (RightToLeft, TopToBottom) xs = strictTRMap reverseTR xs
  arrangeLayout (LeftToRight, BottomToTop) xs = reverseTR xs
  arrangeLayout (RightToLeft, BottomToTop) xs = strictTRMapRev reverseTR xs

collage :: ![ImageOffset] ![Image m] !(Host m) -> Image m
collage _       []   (Just img) = img
collage _       []   _          = empty (px 0.0) (px 0.0)
collage offsets imgs host
  #! offsets` = take (length imgs) (offsets ++ repeat (zero, zero))
  = mkImage (Composite { host    = host
                       , compose = AsCollage offsets` imgs
                       })

instance tuneImage StrokeAttr      where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageStrokeAttr      attr) attribs}
instance tuneImage StrokeWidthAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageStrokeWidthAttr attr) attribs}
instance tuneImage XRadiusAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageXRadiusAttr     attr) attribs}
instance tuneImage YRadiusAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageYRadiusAttr     attr) attribs}
instance tuneImage FillAttr        where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageFillAttr        attr) attribs}
instance tuneImage OpacityAttr     where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageFillOpacityAttr attr) attribs}
instance tuneImage DashAttr        where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageDashAttr        attr) attribs}
instance tuneImage MaskAttr        where
  tuneImage image                    attr = {Image | image & mask = Just attr.MaskAttr.mask }
instance tuneImage OnClickAttr     where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnClickAttr     attr) attribs}
instance tuneImage OnMouseDownAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnMouseDownAttr attr) attribs}
instance tuneImage OnMouseUpAttr   where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnMouseUpAttr   attr) attribs}
instance tuneImage OnMouseOverAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnMouseOverAttr attr) attribs}
instance tuneImage OnMouseMoveAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnMouseMoveAttr attr) attribs}
instance tuneImage OnMouseOutAttr  where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnMouseOutAttr  attr) attribs}
instance tuneImage DraggableAttr   where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageDraggableAttr   attr) attribs}

(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tuneImage attr
(<@<) image attr = tuneImage image attr

(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tuneImage attr
(>@>) attr image = tuneImage image attr

tuneIf :: !Bool !(Image m) !(attr m) -> Image m | tuneImage attr
tuneIf True img t = tuneImage img t
tuneIf _    img _ = img

consNameOf :: !(ImageAttr m) -> String
consNameOf (ImageStrokeAttr      _) = "ImageStrokeAttr"
consNameOf (ImageStrokeWidthAttr _) = "ImageStrokeWidthAttr"
consNameOf (ImageXRadiusAttr     _) = "ImageXRadiusAttr"
consNameOf (ImageYRadiusAttr     _) = "ImageYRadiusAttr"
consNameOf (ImageFillAttr        _) = "ImageFillAttr"
consNameOf (ImageFillOpacityAttr _) = "ImageFillOpacityAttr"
consNameOf (ImageOnClickAttr     _) = "ImageOnClickAttr"
consNameOf (ImageOnMouseDownAttr _) = "ImageOnMouseDownAttr"
consNameOf (ImageOnMouseUpAttr   _) = "ImageOnMouseUpAttr"
consNameOf (ImageOnMouseOverAttr _) = "ImageOnMouseOverAttr"
consNameOf (ImageOnMouseMoveAttr _) = "ImageOnMouseMoveAttr"
consNameOf (ImageOnMouseOutAttr  _) = "ImageOnMouseOutAttr"
consNameOf (ImageDraggableAttr   _) = "ImageDraggableAttr"
consNameOf (ImageDashAttr        _) = "ImageDashAttr"

instance < (ImageAttr m) where < a b = consNameOf a < consNameOf b
instance == (ImageAttr m) where == a b = consNameOf a == consNameOf b

instance toSVGColor String where toSVGColor name = SVGColorText name
instance toSVGColor RGB    where toSVGColor {RGB | r, g, b} = SVGRGB r g b

instance zero RGB where
  zero = { r = 0, g = 0, b = 0 }

instance == ImageTag where == (ImageTagUser n1 s1) (ImageTagUser n2 s2) = n1 == n2 && s1 == s2
                           == (ImageTagSystem  s1) (ImageTagSystem  s2) = s1 == s2
                           == _                    _                    = False
instance <  ImageTag where <  (ImageTagUser n1 s1) (ImageTagUser n2 s2) = n1 < n2 || (n1 == n2 && s1 < s2)
                           <  (ImageTagUser _  _)  _                    = True
                           <  (ImageTagSystem  s1) (ImageTagSystem  s2) = s1 < s2
                           <  _                    _                    = False

instance < (a, b) | < a & < b where
  (<) (x1, x2) (y1, y2) = x1 < y1 && x2 < y2

instance == (a, b) | == a & == b where
  (==) (x1, x2) (y1, y2) = x1 == y1 && x2 == y2

tag :: !*ImageTag !(Image m) -> Image m
tag t image=:{Image | tags} = {Image | image & tags = 'DS'.insert t tags}

tagWithSrc :: !*TagSource !(Image m) -> *(!(!Image m, !ImageTag), !*TagSource)
tagWithSrc [(nut, t) : tsrc] img
  = ((tag t img, nut), tsrc)

/** chop n xs = xss:
      @xss consists of the subsequent sub-lists of @xs of length @n.
      The length of the last element of @xss can be less than @n.
*/
chop :: !Int ![a] -> [[a]]
chop n [] = []
chop n xs
  #! (firstN, withoutN) = splitAt n xs
  = [firstN : chop n withoutN]

instance + ImageOffset where
  (+) :: !ImageOffset !ImageOffset -> ImageOffset
  (+) (xal1, yal1) (xal2, yal2) = (xal1 + xal2, yal1 + yal2)

instance == FontDef where
  (==) :: !FontDef !FontDef -> Bool
  (==) fd1 fd2 = fd1 === fd2

instance < FontDef where
  (<) :: !FontDef !FontDef -> Bool
  (<) fd1 fd2 =  fd1.fontfamily  < fd2.fontfamily
             || (fd1.fontysize   < fd2.fontysize
             || (fd1.fontstretch < fd2.fontstretch
             || (fd1.fontstyle   < fd2.fontstyle
             || (fd1.fontvariant < fd2.fontvariant
             || (fd1.fontweight  < fd2.fontweight)))))

derive gEq FontDef

normalFontDef :: !String !Real -> FontDef // (normalFontDef family size) sets all other fields to "normal"
normalFontDef family size
  = {fontfamily = family, fontysize = size, fontstretch = "normal", fontstyle = "normal", fontvariant = "normal", fontweight = "normal"}

