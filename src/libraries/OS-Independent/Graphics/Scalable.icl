implementation module Graphics.Scalable

from StdMisc import abort
from StdFunc import flip
from StdTuple import fst, snd
from StdOrdList import minList, maxList
from StdOverloaded import class toReal
import Data.List
import Data.Maybe
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
from StdBool import &&
import qualified Data.Set as DS
import Text.HTML
from Data.Functor import class Functor (..)

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

class Tagged t where
  getTags :: t -> [ImageTag]

instance Tagged ImageTag where
  getTags x = [x]

instance Tagged [ImageTag] where
  getTags xs = xs

instance Tagged (Image s) where
  getTags img
    | 'DS'.null img.tags = abort "Image has no tags"
    | otherwise          = 'DS'.toList img.tags

instance zero Span where zero = PxSpan zero
instance abs  Span where abs (PxSpan  x) = PxSpan (abs x)
                         abs (AbsSpan x) = AbsSpan x
                         abs span        = AbsSpan span
instance ~    Span where ~ s             = zero - s
instance +    Span where + (PxSpan 0.0)           b                      = b // Identity
                         + a                      (PxSpan 0.0)           = a // Identity
                         + (PxSpan a)             (PxSpan b)             = PxSpan (a + b)
                         + (PxSpan a)             (AddSpan (PxSpan b) c) = AddSpan (PxSpan (a + b)) c // Associativity
                         + (PxSpan a)             (AddSpan b (PxSpan c)) = AddSpan (PxSpan (a + c)) b // Associativity + commutativity
                         + (AddSpan a (PxSpan b)) (PxSpan c)             = AddSpan a (PxSpan (b + c)) // Associativity
                         + (AddSpan (PxSpan a) b) (PxSpan c)             = AddSpan b (PxSpan (a + c)) // Associativity + commutativity
                         + s                      t                      = AddSpan s t
instance -    Span where - a            (PxSpan 0.0)  = a // Identity
                         - (PxSpan a)   (PxSpan b)    = PxSpan (a - b)
                         - s            t             = SubSpan s t
instance *.   Int  where *. l                       r = toInt (toReal l * toReal r)
instance *.   Real where *. l                       r = l * toReal r
instance *.   Span where *. (PxSpan  a)             k = PxSpan    (a * toReal k)
                         *. (MulSpan (PxSpan k1) a) k = MulSpan a (PxSpan (toReal k * k1))
                         *. (MulSpan a (PxSpan k1)) k = MulSpan a (PxSpan (toReal k * k1))
                         *. (DivSpan a (PxSpan k1)) k = MulSpan a (PxSpan (toReal k / k1))
                         *. s                       k = MulSpan s (PxSpan (toReal k))
instance /.   Int  where /. l                       r = toInt (toReal l / toReal r)
instance /.   Real where /. l                       r = l / toReal r
instance /.   Span where /. (PxSpan  a)             k = PxSpan (a / toReal k)
                         /. (MulSpan a (PxSpan k1)) k = MulSpan a (PxSpan (k1 / toReal k))
                         /. (DivSpan a (PxSpan k1)) k = DivSpan a (PxSpan (k1 * toReal k))
                         /. s                       k = DivSpan s (PxSpan (toReal k))

instance ToSpan Int where
  toSpan n = px (toReal n)

instance ToSpan Real where
  toSpan n = px n

instance ToSpan Span where
  toSpan n = n

minSpan :: ![Span] -> Span
minSpan []  = zero
minSpan [x] = toSpan x
minSpan spans
  #! spans`        = flattenMinSpans spans
  #! (pxs, others) = partition isPxSpan spans`
  | isEmpty others = minPxs pxs
  | isEmpty pxs    = MinSpan others
  | otherwise      = MinSpan [minPxs pxs : others]
  where
  minPxs :: ![Span] -> Span
  minPxs pxs = PxSpan (minList [x \\ PxSpan x <- pxs])

  flattenMinSpans :: ![Span] -> [Span]
  flattenMinSpans []              = []
  flattenMinSpans [MinSpan os:xs] = flattenMinSpans xs ++ os
  flattenMinSpans [x:xs]          = [x:flattenMinSpans xs]

maxSpan :: ![Span] -> Span
maxSpan []  = zero
maxSpan [x] = toSpan x
maxSpan spans
  #! spans`        = flattenMaxSpans spans
  #! (pxs, others) = partition isPxSpan spans`
  | isEmpty others = maxPxs pxs
  | isEmpty pxs    = MaxSpan others
  | otherwise      = MaxSpan [maxPxs pxs : others]
  where
  maxPxs :: ![Span] -> Span
  maxPxs pxs = PxSpan (maxList [x \\ PxSpan x <- pxs])

  flattenMaxSpans :: ![Span] -> [Span]
  flattenMaxSpans []              = []
  flattenMaxSpans [MaxSpan os:xs] = flattenMaxSpans xs ++ os
  flattenMaxSpans [x:xs]          = [x:flattenMaxSpans xs]

mkImage :: !(ImageContent m) -> Image m
mkImage cnt =
  { content             = cnt
  , mask                = Nothing
  , attribs             = 'DS'.newSet
  , transform           = []
  , tags                = 'DS'.newSet
  , totalSpan           = (px 0.0, px 0.0)
  , margin              = (px 0.0, px 0.0, px 0.0, px 0.0)
  , transformCorrection = (px 0.0, px 0.0)
  }

class margin a where
  margin :: !a !(Image m) -> Image m

instance margin Span where
  margin sp im = margin (sp, sp, sp, sp) im

instance margin (Span, Span) where
  margin (sp1, sp2) im = margin (sp1, sp2, sp1, sp2) im

instance margin (Span, Span, Span) where
  margin (sp1, sp2, sp3) im = margin (sp1, sp2, sp3, sp2) im

instance margin (Span, Span, Span, Span) where
  margin (sp1, sp2, sp3, sp4) im = { im & margin = (sp1, sp2, sp3, sp4)}

empty :: !Span !Span -> Image m
empty xspan yspan = mkImage (Basic EmptyImage (maxSpan [zero, xspan], maxSpan [zero, yspan]))

text :: !FontDef !String -> Image m
text font str = mkImage (Basic (TextImage font str) (textxspan font str, px font.FontDef.fontysize))

circle :: !Span -> Image m
circle diameter
  = { mkImage (Basic CircleImage (d, d))
    & attribs = 'DS'.fromList [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              , ImageFillAttr        {fill        = toSVGColor "black"}
                              , ImageFillOpacityAttr {opacity     = 1.0}
                              ]
    }
  where
  d = maxSpan [zero, diameter]

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
  = { mkImage (Line { lineSpan    = (abs (toSpan xspan), abs (toSpan yspan))
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
  = mkImage (Line { lineSpan    = (maxSpan (map fst offsets), maxSpan (map snd offsets))
                  , markers     = markers
                  , lineContent = PolygonImage offsets
                  })

polyline :: !(Maybe (Markers m)) ![ImageOffset] -> Image m
polyline markers offsets
  #! offsets = normalizePolyPoints offsets
  = { mkImage (Line { lineSpan    = (maxSpan (map fst offsets), maxSpan (map snd offsets))
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
  #! minX = minSpan (map fst offsets)
  #! minY = minSpan (map snd offsets)
  = foldr (\(x, y) acc -> [(x - minX, y - minY) : acc]) [] offsets

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

rad :: !Real -> Angle
rad r = Rad r

deg :: !Real -> Angle
deg d = Deg d

pi =: 3.14159265359

overlay :: ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
overlay _      _       []   (Just img) = img
overlay _      _       []   _          = empty (px 0.0) (px 0.0)
overlay aligns offsets imgs host
  #! l = length imgs
  = mkImage (Composite { offsets = take l (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsOverlay (take l (aligns ++ repeat (AtLeft, AtTop))) imgs
                       })

beside :: ![YAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
beside ylayouts offsets imgs host
  #! l = length imgs
  = grid (Rows 1) (LeftToRight, TopToBottom) (take l [(AtLeft, ylayout) \\ ylayout <- ylayouts]) (take l offsets) imgs host

above :: ![XAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
above xlayouts offsets imgs host
  #! l = length imgs
  = grid (Columns 1) (LeftToRight, TopToBottom) (take l [(xlayout, AtTop) \\ xlayout <- xlayouts]) (take l offsets) imgs host

grid :: !GridDimension !GridLayout ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid _ _ _ _ [] (Just img) = img
grid _ _ _ _ [] _          = empty (px 0.0) (px 0.0)
grid dimension layout aligns offsets imgs host
  #! noOfImgs     = length imgs
  #! (cols, rows) = case dimension of
                      Rows no
                        #! no` = max 1 no
                        = (noOfImgs / no` + sign (noOfImgs rem no`), no`)
                      Columns no
                        #! no` = max 1 no
                        = (no`, noOfImgs / no` + sign (noOfImgs rem no`))
  #! imgsComplete = imgs ++ repeatn (cols * rows - noOfImgs) (empty (px 0.0) (px 0.0))
  #! imgs`        = arrangeLayout layout (if (isRowMajor dimension)
                                            (chop cols imgsComplete)
                                            [map (flip (!!) i) (chop rows imgsComplete) \\ i <- [0 .. rows - 1]]
                                         )
  = mkImage (Composite { offsets = take noOfImgs (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsGrid (cols, rows) (take noOfImgs (aligns ++ repeat (AtLeft, AtTop))) imgs`
                       })
  where
  isRowMajor :: !GridDimension -> Bool
  isRowMajor (Rows _) = True
  isRowMajor _        = False

  arrangeLayout :: !GridLayout ![[a]] -> [[a]]
  arrangeLayout (LeftToRight, TopToBottom) xs = xs
  arrangeLayout (RightToLeft, TopToBottom) xs = map reverse xs
  arrangeLayout (LeftToRight, BottomToTop) xs = reverse xs
  arrangeLayout (RightToLeft, BottomToTop) xs = reverse (map reverse xs)

collage :: ![ImageOffset] ![Image m] !(Host m) -> Image m
collage _       []   (Just img) = img
collage _       []   _          = empty (px 0.0) (px 0.0)
collage offsets imgs host
  = mkImage (Composite { offsets = take (length imgs) (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsCollage imgs
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
instance tuneImage OnClickAttr     where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageOnClickAttr     attr) attribs}
instance tuneImage DashAttr        where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = 'DS'.insert (ImageDashAttr        attr) attribs}
instance tuneImage MaskAttr        where
  tuneImage image                    attr = {Image | image & mask = Just attr.MaskAttr.mask }

(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tuneImage attr
(<@<) image attr = tuneImage image attr

(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tuneImage attr
(>@>) attr image = tuneImage image attr

consNameOf :: !(ImageAttr m) -> String
consNameOf (ImageStrokeAttr      _) = "ImageStrokeAttr"
consNameOf (ImageStrokeWidthAttr _) = "ImageStrokeWidthAttr"
consNameOf (ImageXRadiusAttr     _) = "ImageXRadiusAttr"
consNameOf (ImageYRadiusAttr     _) = "ImageYRadiusAttr"
consNameOf (ImageFillAttr        _) = "ImageFillAttr"
consNameOf (ImageFillOpacityAttr _) = "ImageFillOpacityAttr"
consNameOf (ImageOnClickAttr     _) = "ImageOnClickAttr"
consNameOf (ImageDashAttr        _) = "ImageDashAttr"

instance < (ImageAttr m) where < a b = consNameOf a < consNameOf b
instance == (ImageAttr m) where == a b = consNameOf a == consNameOf b

instance toSVGColor String where toSVGColor name = SVGColorText name
instance toSVGColor RGB    where toSVGColor {RGB | r, g, b} = SVGRGB r g b

instance zero RGB where
  zero = { r = 0, g = 0, b = 0 }

instance imageTag Int    where imageTag n = ImageTagInt n
instance imageTag String where imageTag s = ImageTagString s
instance == ImageTag     where == (ImageTagInt    n1) (ImageTagInt    n2) = n1 == n2
                               == (ImageTagString s1) (ImageTagString s2) = s1 == s2
                               == (ImageTagSystem s1) (ImageTagSystem s2) = s1 == s2
                               == _                   _                   = False
instance <  ImageTag     where <  (ImageTagInt    n1) (ImageTagInt    n2) = n1 < n2
                               <  (ImageTagInt    _)  _                   = True
                               <  (ImageTagString s1) (ImageTagString s2) = s1 < s2
                               <  (ImageTagString _)  (ImageTagSystem _)  = True
                               <  (ImageTagSystem s1) (ImageTagSystem s2) = s1 < s2
                               <  _                   _                   = False

instance < (a, b) | < a & < b where
  (<) (x1, x2) (y1, y2) = x1 < y1 && x2 < y2

instance == (a, b) | == a & == b where
  (==) (x1, x2) (y1, y2) = x1 == y1 && x2 == y2

tag :: !t !(Image m) -> Image m | Tagged t
tag ts image=:{Image | tags} = {Image | image & tags = 'DS'.union tags ('DS'.fromList (getTags ts))}

/** updateOrAdd c x xs = ys:
      @xs must be a sorted list. @ys replaces the first element y in @xs for which (@c @x y) is valid with @x.
      If such an element is not found, then @x is added to @xs.
      @ys is also a sorted list if @c respects the ordering relation.
*/
updateOrAdd :: !(a a -> Bool) !a ![a] -> [a] | < a
updateOrAdd c x [] = [x]
updateOrAdd c x yys=:[y:ys]
  | y < x     = [y : updateOrAdd c x ys]
  | c x y     = [x : ys]
  | otherwise = [x : yys]

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

:: Angle
  = Deg !Real
  | Rad !Real

toDeg :: !Angle -> Real
toDeg (Deg r) = r
toDeg (Rad r) = r / (pi / 180.0)

toRad :: !Angle -> Real
toRad (Deg r) = (pi / 180.0) * r
toRad (Rad r) = r

normalize :: !Angle -> Angle
normalize a
  #! a`    = toDeg a
  #! absa` = abs a`
  | absa` <= 360.0 = Deg a`
  | a`    >  0.0   = Deg (a` - d absa`)
  | otherwise      = Deg (a` + d absa`)
  where
  d :: !Real -> Real
  d absa` = toReal (entier (absa` / 360.0)) * 360.0

instance == Angle where
  (==) :: !Angle !Angle -> Bool
  (==) (Deg r) r` = r == toDeg r`
  (==) (Rad r) r` = r == toRad r`

instance < Angle where
  (<) :: !Angle !Angle -> Bool
  (<) (Deg r) r` = r < toDeg r`
  (<) (Rad r) r` = r < toRad r`

instance + Angle where
  (+) :: !Angle !Angle -> Angle
  (+) (Deg r) r` = Deg (r + toDeg r`)
  (+) (Rad r) r` = Rad (r + toRad r`)

instance - Angle where
  (-) :: !Angle !Angle -> Angle
  (-) (Deg r) r` = Deg (r - toDeg r`)
  (-) (Rad r) r` = Rad (r - toRad r`)

instance sign Angle where
  sign :: !Angle -> Int
  sign (Deg r) = sign r
  sign (Rad r) = sign r

instance == FontDef where
  (==) :: !FontDef !FontDef -> Bool
  (==) fd1 fd2 = fd1.fontfamily  == fd2.fontfamily
              && fd1.fontysize   == fd2.fontysize
              && fd1.fontstretch == fd2.fontstretch
              && fd1.fontstyle   == fd2.fontstyle
              && fd1.fontvariant == fd2.fontvariant
              && fd1.fontweight  == fd2.fontweight

instance < FontDef where
  (<) :: !FontDef !FontDef -> Bool
  (<) fd1 fd2 = (fd1.fontfamily  == fd2.fontfamily
              && fd1.fontstretch == fd2.fontstretch
              && fd1.fontstyle   == fd2.fontstyle
              && fd1.fontvariant == fd2.fontvariant
              && fd1.fontweight  == fd2.fontweight)
              && fd1.fontysize   <  fd2.fontysize

normalFontDef :: !String !Real -> FontDef // (normalFontDef family size) sets all other fields to "normal"
normalFontDef family size
  = {fontfamily = family, fontysize = size, fontstretch = "normal", fontstyle = "normal", fontvariant = "normal", fontweight = "normal"}

