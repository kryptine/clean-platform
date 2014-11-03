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

imagexspan :: !t -> Span | Tagged t
imagexspan as = LookupSpan (ImageXSpan ('DS'.fromList (getTags as)))

imageyspan :: !t -> Span | Tagged t
imageyspan as = LookupSpan (ImageYSpan ('DS'.fromList (getTags as)))

columnspan :: !t !Int -> Span | Tagged t
columnspan as a = LookupSpan (ColumnXSpan ('DS'.fromList (getTags as)) a)

rowspan :: !t !Int -> Span | Tagged t
rowspan as a = LookupSpan (RowYSpan ('DS'.fromList (getTags as)) a)

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

instance maxOf Span where
  maxOf xs = maxSpan xs

instance maxOf Real where
  maxOf xs = maxList xs

instance maxOf Int where
  maxOf xs = maxList xs

instance minOf Span where
  minOf xs = minSpan xs

instance minOf Real where
  minOf xs = minList xs

instance minOf Int where
  minOf xs = minList xs

instance IsSpan Int where
  toSpan n = px (toReal n)

instance IsSpan Real where
  toSpan n = px n

instance IsSpan Span where
  toSpan n = n

minSpan :: ![s] -> Span | IsSpan s
minSpan []  = zero
minSpan [x] = toSpan x
minSpan spans
  | isEmpty others = minPxs
  | isEmpty pxs    = MinSpan others
  | otherwise      = MinSpan [minPxs : others]
  where
  spans`        = flattenMinSpans (map toSpan spans)
  flattenMinSpans []              = []
  flattenMinSpans [MinSpan os:xs] = flattenMinSpans xs ++ os
  flattenMinSpans [x:xs]          = [x:flattenMinSpans xs]
  (pxs, others) = partition isPxSpan spans`
  minPxs        = PxSpan (minList [x \\ PxSpan x <- pxs])

maxSpan :: ![s] -> Span | IsSpan s
maxSpan []  = zero
maxSpan [x] = toSpan x
maxSpan spans
  | isEmpty others = maxPxs
  | isEmpty pxs    = MaxSpan others
  | otherwise      = MaxSpan [maxPxs : others]
  where
  spans`        = flattenMaxSpans (map toSpan spans)
  flattenMaxSpans []              = []
  flattenMaxSpans [MaxSpan os:xs] = flattenMaxSpans xs ++ os
  flattenMaxSpans [x:xs]          = [x:flattenMaxSpans xs]
  (pxs, others) = partition isPxSpan spans`
  maxPxs        = PxSpan (maxList [x \\ PxSpan x <- pxs])

mkImage :: (ImageContent m) -> Image m
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

instance margin (a, b) | IsSpan a & IsSpan b where
  margin (sp1, sp2) im = margin (toSpan sp1, toSpan sp2, toSpan sp1, toSpan sp2) im

instance margin (a, b, c) | IsSpan a & IsSpan b & IsSpan c where
  margin (sp1, sp2, sp3) im = margin (toSpan sp1, toSpan sp2, toSpan sp3, toSpan sp2) im

instance margin (a, b, c, d) | IsSpan a & IsSpan b & IsSpan c & IsSpan d where
  margin (sp1, sp2, sp3, sp4) im = { im & margin = (toSpan sp1, toSpan sp2, toSpan sp3, toSpan sp4)}

instance margin Int where
  margin sp im = margin (toSpan sp) im

instance margin Real where
  margin sp im = margin (toSpan sp) im

empty :: !s !s -> Image m | IsSpan s
empty xspan yspan = mkImage (Basic EmptyImage (maxSpan [zero, xspan], maxSpan [zero, yspan]))

text :: !FontDef !String -> Image m
text font str = mkImage (Basic (TextImage font str) (textxspan font str, px font.FontDef.fontysize))

circle :: !s -> Image m | IsSpan s
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

ellipse :: !s !s -> Image m | IsSpan s
ellipse diax diay
  = { mkImage (Basic EllipseImage (maxSpan [zero, diax], maxSpan [zero, diay]))
    & attribs = 'DS'.fromList [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                              , ImageStrokeWidthAttr {strokewidth = px 1.0}
                              , ImageFillAttr        {fill        = toSVGColor "black"}
                              , ImageFillOpacityAttr {opacity     = 1.0}
                              ]
    }

rect :: !s !s -> Image m | IsSpan s
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

xline :: !(Maybe (Markers m)) !s -> Image m | IsSpan s
xline markers xspan = line markers Slash xspan zero

yline :: !(Maybe (Markers m)) !s -> Image m | IsSpan s
yline markers yspan = line markers Slash zero yspan

line :: !(Maybe (Markers m)) !Slash !s !s -> Image m | IsSpan s
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
  # offsets = normalizePolyPoints offsets
  = mkImage (Line { lineSpan    = (maxSpan (map fst offsets), maxSpan (map snd offsets))
                  , markers     = markers
                  , lineContent = PolygonImage offsets
                  })

polyline :: !(Maybe (Markers m)) ![ImageOffset] -> Image m
polyline markers offsets
  # offsets = normalizePolyPoints offsets
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
  # minX = minSpan (map fst offsets)
  # minY = minSpan (map snd offsets)
  = foldr (\(x, y) acc -> [(x - minX, y - minY) : acc]) [] offsets

rotate :: !th !(Image m) -> Image m | Angle th
rotate a image=:{Image | transform = ts}
  | a` == zero = image
  | otherwise  = {Image | image & transform = ts`}
  where
  a`  = toDeg (normalize a)
  ts` = case ts of
          [RotateImage angle : ts]
            # a` = normalize (toDeg angle + toDeg a)
            = if (a` == zero) ts [RotateImage a` : ts]
          ts
            = [RotateImage a` : ts]

fit :: !s !s !(Image m) -> Image m | IsSpan s
fit xspan yspan image=:{Image | transform = ts}
  = {Image | image & transform = ts`}
  where
  xspan` = maxSpan [zero, xspan]
  yspan` = maxSpan [zero, yspan]
  ts`    = case ts of
             [FitImage _ _ : ts] = [FitImage xspan` yspan` : ts]
             ts                  = [FitImage xspan` yspan` : ts]

fitx :: !s !(Image m) -> Image m | IsSpan s
fitx xspan image=:{Image | transform = ts}
  = {Image | image & transform = ts`}
  where
  xspan` = maxSpan [zero, xspan]
  ts`    = case ts of
             [FitXImage _ : ts] = [FitXImage xspan` : ts]
             [FitYImage _ : ts] = [FitXImage xspan` : ts]
             ts                 = [FitXImage xspan` : ts]

fity :: !s !(Image m) -> Image m | IsSpan s
fity yspan image=:{Image | transform = ts}
  = {Image | image & transform = ts`}
  where
  yspan` = maxSpan [zero, yspan]
  ts`    = case ts of
             [FitXImage _ : ts] = [FitYImage yspan` : ts]
             [FitYImage _ : ts] = [FitYImage yspan` : ts]
             ts                 = [FitYImage yspan` : ts]

skewx :: !th !(Image m) -> Image m | Angle th
skewx xskew image=:{Image | transform = ts}
  | xskew` == zero = image
  | otherwise      = {Image | image & transform = ts`}
  where
  xskew` = toDeg (normalize xskew)
  ts`    = case ts of
             [SkewXImage a : ts]
               # a` = normalize (a + toDeg xskew)
               = if (a` == zero) ts [SkewXImage a` : ts]
             ts
               = [SkewXImage xskew` : ts]

skewy :: !th !(Image m) -> Image m | Angle th
skewy yskew image=:{Image | transform = ts}
  | yskew` == zero = image
  | otherwise      = {Image | image & transform = ts`}
  where
  yskew` = toDeg (normalize yskew)
  ts`    = case ts of
            [SkewYImage a : ts]
              # a` = normalize (a + toDeg yskew)
              = if (a` == zero) ts [SkewYImage a` : ts]
            ts
              = [SkewYImage yskew` : ts]

radian :: !Real -> Rad
radian r = Rad r

degree :: !Real -> Deg
degree d = Deg d

pi =: 3.14159265359

overlay :: ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
overlay _      _       []   (Just img) = img
overlay _      _       []   _          = empty 0 0
overlay aligns offsets imgs host
  # l = length imgs
  = mkImage (Composite { offsets = take l (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsOverlay (take l (aligns ++ repeat (AtLeft, AtTop))) imgs
                       })

beside :: ![YAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
beside ylayouts offsets imgs host
  # l = length imgs
  = grid (Rows 1) (LeftToRight, TopToBottom) (take l [(AtLeft, ylayout) \\ ylayout <- ylayouts]) (take l offsets) imgs host

above :: ![XAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
above xlayouts offsets imgs host
  # l = length imgs
  = grid (Columns 1) (LeftToRight, TopToBottom) (take l [(xlayout, AtTop) \\ xlayout <- xlayouts]) (take l offsets) imgs host

grid :: !GridDimension !GridLayout ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid _ _ _ _ [] (Just img) = img
grid _ _ _ _ [] _          = empty 0 0
grid dimension layout aligns offsets imgs host
  = mkImage (Composite { offsets = take noOfImgs (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsGrid (cols, rows) (take noOfImgs (aligns ++ repeat (AtLeft, AtTop))) imgs`
                       })
  where
  noOfImgs     = length imgs
  (cols, rows) = case dimension of
                   Rows    no = let no` = max 1 no
                                in (noOfImgs / no` + sign (noOfImgs rem no`), no`)
                   Columns no = let no` = max 1 no
                                in (no`, noOfImgs / no` + sign (noOfImgs rem no`))
  imgsComplete = imgs ++ repeatn (cols * rows - noOfImgs) (empty 0 0)
  imgs`        = arrangeLayout layout (if (isRowMajor dimension)
                                         (chop cols imgsComplete)
                                         [map (flip (!!) i) (chop rows imgsComplete) \\ i <- [0 .. rows - 1]]
                                      )

  isRowMajor :: GridDimension -> Bool
  isRowMajor (Rows _) = True
  isRowMajor _        = False

  arrangeLayout :: !GridLayout [[a]] -> [[a]]
  arrangeLayout (LeftToRight, TopToBottom) xs = xs
  arrangeLayout (RightToLeft, TopToBottom) xs = map reverse xs
  arrangeLayout (LeftToRight, BottomToTop) xs = reverse xs
  arrangeLayout (RightToLeft, BottomToTop) xs = reverse (map reverse xs)

collage :: ![ImageOffset] ![Image m] !(Host m) -> Image m
collage _       []   (Just img) = img
collage _       []   _          = empty 0 0
collage offsets imgs host
  = mkImage (Composite { offsets = take (length imgs) (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsCollage imgs
                       })

(@$) infixr :: !(Mask m) !(Image m) -> Image m
(@$) mask orig = maskWith mask orig

($@) infixl :: !(Image m) !(Mask m) -> Image m
($@) orig mask = maskWith mask mask

maskWith :: !(Mask m) !(Image m) -> Image m
maskWith mask orig = { orig & mask = Just mask }

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

tags :: !(Image m) -> [ImageTag]
tags image=:{Image | tags} = 'DS'.toList tags

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
chop n xs = [firstN : chop n withoutN]
  where
  (firstN, withoutN) = splitAt n xs

instance + ImageOffset where
  (+) (xal1, yal1) (xal2, yal2) = (xal1 + xal2, yal1 + yal2)

instance Angle Deg where
  toDeg  r       = r
  toRad  (Deg r) = Rad ((pi / 180.0) * r)
  normalize a
    | absa` <= 360.0 = a
    | a`    >  0.0   = Deg (a` - d)
    | otherwise      = Deg (a` + d)
    where
    a`    = toReal a
    absa` = abs a`
    d     = toReal (entier (absa` / 360.0)) * 360.0

instance toReal Deg where
  toReal (Deg r) = r

instance == Deg where
  (==) (Deg r) (Deg r`) = r == r`

instance < Deg where
  (<) (Deg r) (Deg r`) = r < r`

instance + Deg where
  (+) (Deg r) (Deg r`) = Deg (r + r`)

instance - Deg where
  (-) (Deg r) (Deg r`) = Deg (r - r`)

instance zero Deg where
  zero = Deg 0.0

instance sign Deg where
  sign (Deg r) = sign r

instance Angle Rad where
  toDeg  (Rad r) = Deg (r / (pi / 180.0))
  toRad  r       = r
  normalize r    = toRad (normalize (toDeg r))

instance toReal Rad where
  toReal (Rad r) = r

instance == Rad where
  (==) (Rad r) (Rad r`) = r == r`

instance < Rad where
  (<) (Rad r) (Rad r`) = r < r`

instance + Rad where
  (+) (Rad r) (Rad r`) = Rad (r + r`)

instance - Rad where
  (-) (Rad r) (Rad r`) = Rad (r - r`)

instance zero Rad where
  zero = Rad 0.0

instance sign Rad where
  sign (Rad r) = sign r

instance == FontDef where
  (==) fd1 fd2 = fd1.fontfamily  == fd2.fontfamily
              && fd1.fontysize   == fd2.fontysize
              && fd1.fontstretch == fd2.fontstretch
              && fd1.fontstyle   == fd2.fontstyle
              && fd1.fontvariant == fd2.fontvariant
              && fd1.fontweight  == fd2.fontweight

instance < FontDef where
  (<) fd1 fd2 = (fd1.fontfamily  == fd2.fontfamily
              && fd1.fontstretch == fd2.fontstretch
              && fd1.fontstyle   == fd2.fontstyle
              && fd1.fontvariant == fd2.fontvariant
              && fd1.fontweight  == fd2.fontweight)
              && fd1.fontysize   <  fd2.fontysize
