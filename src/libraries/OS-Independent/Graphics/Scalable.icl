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

imagexspan :: ![ImageTag] -> Span
imagexspan as = LookupSpan (ImageXSpan ('DS'.fromList as))

imageyspan :: ![ImageTag] -> Span
imageyspan as = LookupSpan (ImageYSpan ('DS'.fromList as))

columnspan :: ![ImageTag] !Int -> Span
columnspan as a = LookupSpan (ColumnXSpan ('DS'.fromList as) a)

rowspan :: ![ImageTag] !Int -> Span
rowspan as a = LookupSpan (RowYSpan ('DS'.fromList as) a)

instance zero Span where zero                    = PxSpan zero
instance one  Span where one                     = PxSpan one
instance +    Span where + (PxSpan 0.0)           b                      = b // Identity
                         + a                      (PxSpan 0.0)           = a // Identity
                         + (PxSpan a)             (PxSpan b)             = PxSpan (a + b)
                         + (PxSpan a)             (AddSpan (PxSpan b) c) = AddSpan (PxSpan (a + b)) c // Associativity
                         + (PxSpan a)             (AddSpan b (PxSpan c)) = AddSpan (PxSpan (a + c)) b // Associativity + commutativity
                         + (AddSpan a (PxSpan b)) (PxSpan c)             = AddSpan a (PxSpan (b + c)) // Associativity
                         + (AddSpan (PxSpan a) b) (PxSpan c)             = AddSpan b (PxSpan (a + c)) // Associativity + commutativity
                         + s                      t                      = AddSpan s t
instance -    Span where - a          (PxSpan 0.0) = a // Identity
                         - (PxSpan a) (PxSpan b)   = PxSpan (a - b)
                         - s          t            = SubSpan s t
instance abs  Span where abs (PxSpan  x)         = PxSpan (abs x)
                         abs (AbsSpan x)         = AbsSpan x
                         abs span                = AbsSpan span
instance ~    Span where ~ s                     = zero - s
instance *.   Int  where *. l              r     = toInt (toReal l * toReal r)
instance *.   Real where *. l              r     = l * toReal r
instance *.   Span where *. (PxSpan  a)    k     = PxSpan    (a * toReal k)
                         *. (MulSpan a k1) k     = MulSpan a (toReal k * k1)
                         *. (DivSpan a k1) k     = MulSpan a (toReal k / k1)
                         *. s              k     = MulSpan s (toReal k)
instance /.   Int  where /. l              r     = toInt (toReal l / toReal r)
instance /.   Real where /. l              r     = l / toReal r
instance /.   Span where /. (PxSpan  a)    k     = PxSpan    (a / toReal k)
                         /. (MulSpan a k1) k     = MulSpan a (k1 / toReal k)
                         /. (DivSpan a k1) k     = DivSpan a (k1 * toReal k)
                         /. s              k     = DivSpan s (toReal k)

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

minSpan :: ![Span] -> Span
minSpan []  = zero
minSpan [x] = x
minSpan spans
  | isEmpty others = minPxs
  | isEmpty pxs    = MinSpan others
  | otherwise      = MinSpan [minPxs : others]
  where
  spans`        = flattenMinSpans spans
  flattenMinSpans []              = []
  flattenMinSpans [MinSpan os:xs] = flattenMinSpans xs ++ os
  flattenMinSpans [x:xs]          = [x:flattenMinSpans xs]
  (pxs, others) = partition isPxSpan spans`
  minPxs        = PxSpan (minList [x \\ PxSpan x <- pxs])

maxSpan :: ![Span] -> Span
maxSpan []  = zero
maxSpan [x] = x
maxSpan spans
  | isEmpty others = maxPxs
  | isEmpty pxs    = MaxSpan others
  | otherwise      = MaxSpan [maxPxs : others]
  where
  spans`        = flattenMaxSpans spans
  flattenMaxSpans []              = []
  flattenMaxSpans [MaxSpan os:xs] = flattenMaxSpans xs ++ os
  flattenMaxSpans [x:xs]          = [x:flattenMaxSpans xs]
  (pxs, others) = partition isPxSpan spans`
  maxPxs        = PxSpan (maxList [x \\ PxSpan x <- pxs])

mkImage :: (ImageContent m) -> Image m
mkImage cnt =
  { content             = cnt
  , mask                = Nothing
  , attribs             = []
  , transform           = []
  , tags                = 'DS'.newSet
  , totalSpan           = (px 0.0, px 0.0)
  , margin              = (px 0.0, px 0.0, px 0.0, px 0.0)
  , transformCorrection = (px 0.0, px 0.0)
  , connectors          = []
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
  margin sps im = { im & margin = sps }

empty :: !Span !Span -> Image m
empty xspan yspan = mkImage (Basic EmptyImage (maxSpan [zero, xspan], maxSpan [zero, yspan]))

text :: !FontDef !String -> Image m
text font str = mkImage (Basic (TextImage font str) (textxspan font str, font.FontDef.fontyspan))

circle :: !Span -> Image m
circle diameter
  = { mkImage (Basic CircleImage (d, d))
    & attribs = [ ImageStrokeWidthAttr {strokewidth = px 0.0}
                , ImageFillAttr        {fill        = toSVGColor "black"}
                , ImageFillOpacityAttr {opacity     = 1.0}
                ]
    }
  where
  d = maxSpan [zero, diameter]

ellipse :: !Span !Span -> Image m
ellipse diax diay
  = { mkImage (Basic EllipseImage (maxSpan [zero, diax], maxSpan [zero, diay]))
    & attribs = [ ImageStrokeWidthAttr {strokewidth = px 0.0}
                , ImageFillAttr        {fill        = toSVGColor "black"}
                , ImageFillOpacityAttr {opacity     = 1.0}
                ]
    }

rect :: !Span !Span -> Image m
rect xspan yspan
  = { mkImage (Basic RectImage (maxSpan [zero, xspan], maxSpan [zero, yspan]))
    & attribs = [ ImageStrokeWidthAttr {strokewidth = px 0.0}
                , ImageFillAttr        {fill        = toSVGColor "black"}
                , ImageFillOpacityAttr {opacity     = 1.0}
                ]
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
    & attribs = [ ImageStrokeAttr      {stroke      = toSVGColor "black"}
                , ImageStrokeWidthAttr {strokewidth = px 1.0}
                , ImageFillAttr        {fill        = toSVGColor "black"}
                , ImageFillOpacityAttr {opacity     = 1.0}
                ]
    }

polygon :: !(Maybe (Markers m)) ![ImageOffset] -> Image m
polygon markers offsets
  = mkImage (Line { lineSpan    = (maxSpan (map fst offsets), maxSpan (map snd offsets))
                  , markers     = markers
                  , lineContent = PolygonImage offsets
                  })

polyline :: !(Maybe (Markers m)) ![ImageOffset] -> Image m
polyline markers offsets
  = { mkImage (Line { lineSpan    = (maxSpan (map fst offsets), maxSpan (map snd offsets))
                    , markers     = markers
                    , lineContent = PolylineImage offsets
                    })
    & attribs = [ ImageFillAttr        {fill        = toSVGColor "none"}
                , ImageStrokeAttr      {stroke      = toSVGColor "black"}
                , ImageStrokeWidthAttr {strokewidth = px 1.0}
                ]
    }

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

fit :: !Span !Span !(Image m) -> Image m
fit xspan yspan image=:{Image | transform = ts}
  = {Image | image & transform = ts`}
  where
  xspan` = maxSpan [zero, xspan]
  yspan` = maxSpan [zero, yspan]
  ts`    = case ts of
             [FitImage _ _ : ts] = [FitImage xspan` yspan` : ts]
             ts                  = [FitImage xspan` yspan` : ts]

fitx :: !Span !(Image m) -> Image m
fitx xspan image=:{Image | transform = ts}
  = {Image | image & transform = ts`}
  where
  xspan` = maxSpan [zero,xspan]
  ts`    = case ts of
             [FitXImage _ : ts] = [FitXImage xspan` : ts]
             [FitYImage _ : ts] = [FitXImage xspan` : ts]
             ts                 = [FitXImage xspan` : ts]

fity :: !Span !(Image m) -> Image m
fity yspan image=:{Image | transform = ts}
  = {Image | image & transform = ts`}
  where
  yspan` = maxSpan [zero,yspan]
  ts`    = case ts of
             [FitXImage _ : ts] = [FitYImage yspan` : ts]
             [FitYImage _ : ts] = [FitYImage yspan` : ts]
             ts                 = [FitYImage yspan` : ts]

applyTransforms :: ![ImageTransform] !ImageSpan -> (ImageSpan, ImageOffset)
applyTransforms ts sp = foldr f (sp, (px 0.0, px 0.0)) ts
  where
  f (RotateImage th)   (accSp          , accOff)
    # (imSp, offs) = rotatedImageSpan th accSp
    = (imSp, accOff + offs)
  f (SkewXImage th)    (accSp=:(_, ysp), (xoff, yoff))
    # (xsp, offs) = skewXImageWidth th accSp
    = ((xsp, ysp), (xoff + offs, yoff))
  f (SkewYImage th)    (accSp=:(xsp, _), (xoff, yoff))
    # (ysp, offs) = skewYImageHeight th accSp
    = ((xsp, ysp), (xoff, yoff + offs))
  f (FitImage xsp ysp) (_              , accOff) = ((xsp, ysp), accOff)
  f (FitXImage sp)     ((_, ysp)       , accOff) = ((sp, ysp),  accOff)
  f (FitYImage sp)     ((xsp, _)       , accOff) = ((xsp, sp),  accOff)

// Rotates a rectangle by a given angle. Currently, this function is rather
// naive. It rotates the rectangle (usually the bounding box of a shape) around
// its centre point. It returns the span of the entire rotated image, i.e., the
// new bounding box of the rotated image. If you rotate a square by, e.g. 45
// degrees, then the resulting bounding box will be larger than the original
// square bounding box. If you rotate the square again by 45 degrees, you would
// expect that the bounding box after the second rotation is as big as the
// original square again. However, with this particular function, the
// resulting bounding box is bigger still, because the new bounding box was
// rotated.
//
// @param th | Angle th      angle          The angle of rotation
// @param (a, a) | IsSpan a  (xspan, yspan) The original x and y spans of the
//                                          non-rotated image
// @return ((a, a), (a, a)) | IsSpan a      The span of the rotated image and
//                                          the offset from between the old and
//                                          new top-left corner of the bounding
//                                          box
rotatedImageSpan :: !th !(a, a) -> ((a, a), (a, a)) | Angle th & IsSpan a
rotatedImageSpan angle (xspan, yspan)
  = ( (abs (maxAllX - minAllX), abs (maxAllY - minAllY))
    , (zero - minAllX, zero - minAllY))
  where
  cx        = xspan /. 2.0
  cy        = yspan /. 2.0
  allPoints = [ mkTransform zero  zero
              , mkTransform xspan zero
              , mkTransform zero  yspan
              , mkTransform xspan yspan ]
  allX      = map fst allPoints
  maxAllX   = maxOf allX
  minAllX   = minOf allX
  allY      = map snd allPoints
  maxAllY   = maxOf allY
  minAllY   = minOf allY
  angle`    = toReal (toRad angle)
  mkTransform x y = ( cx + (x - cx) *. cos angle` + (y - cy) *. sin angle`
                    , cy - (x - cx) *. sin angle` + (y - cy) *. cos angle`)

// Skew an image by a given angle. This function is naive as well, for the same
// reasons as the rotation function.
// TODO : We need to calculate the difference between the original and skewed
// top-left coordinate here as well, because we need it in grid layouts
//
// @param (th | Angle th)     angle          The skew angle
// @param ((a, a) | IsSpan a) (xspan, yspan) The original x and y spans of the
//                                           non-skewed image
// @return ((a, a) | IsSpan a) The new width of the skewed image and possible offset
skewXImageWidth :: !th !(a, a) -> (a, a) | Angle th & IsSpan a
skewXImageWidth angle (xspan, yspan) = (newXSpan, mkOffset)
  where
  rAngle   = toReal (toRad angle)
  newXSpan = xspan + (abs (yspan *. tan rAngle))
  spanDiff = newXSpan - xspan
  mkOffset
    | rAngle <= 0.0 = zero - spanDiff
    | otherwise     = spanDiff

// Skew an image by a given angle. This function is naive as well, for the same
// reasons as the rotation function.
// TODO : We need to calculate the difference between the original and skewed
// top-left coordinate here as well, because we need it in grid layouts
//
// @param (th | Angle th)     angle          The skew angle
// @param ((a, a) | IsSpan a) (xspan, yspan) The original x and y spans of the
//                                           non-skewed image
// @return ((a, a) | IsSpan a) The new height of the skewed image and possible offset
skewYImageHeight :: !th !(a, a) -> (a, a) | Angle th & IsSpan a
skewYImageHeight angle (xspan, yspan) = (newYSpan, mkOffset)
  where
  rAngle   = toReal (toRad angle)
  newYSpan = yspan + (abs (xspan *. tan rAngle))
  spanDiff = newYSpan - xspan
  mkOffset
    | rAngle <= 0.0 = zero - spanDiff
    | otherwise     = spanDiff


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

mkEdges :: [Edge] -> Set (Set ImageTag, Set ImageTag)
mkEdges edges = 'DS'.fromList (map (\(xs, ys) -> ('DS'.fromList xs, 'DS'.fromList ys)) edges)

overlay :: ![ImageAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
overlay _      _       []   (Just img) = img
overlay _      _       []   _          = empty zero zero
overlay aligns offsets imgs host
  # l = length imgs
  = mkImage (Composite { offsets = take l (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsOverlay (take l (aligns ++ repeat (AtLeft, AtTop))) imgs
                       , edges   = 'DS'.newSet
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
grid _ _ _ _ [] _          = empty zero zero
grid dimension layout aligns offsets imgs host
  = mkImage (Composite { offsets = take noOfImgs (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsGrid (cols, rows) (take noOfImgs (aligns ++ repeat (AtLeft, AtTop))) imgs`
                       , edges   = 'DS'.newSet
                       })
  where
  noOfImgs     = length imgs
  (cols, rows) = case dimension of
                   Rows    no = let no` = max 1 no
                                in (noOfImgs / no` + sign (noOfImgs rem no`), no`)
                   Columns no = let no` = max 1 no
                                in (no`, noOfImgs / no` + sign (noOfImgs rem no`))
  imgsComplete = imgs ++ repeatn (cols * rows - noOfImgs) (empty zero zero)
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
collage _       []   _          = empty zero zero
collage offsets imgs host
  = mkImage (Composite { offsets = take (length imgs) (offsets ++ repeat (zero, zero))
                       , host    = host
                       , compose = AsCollage imgs
                       , edges   = 'DS'.newSet
                       })

(@$) infixr :: !(Image m) !(Image m) -> Image m
(@$) mask orig = maskWith orig mask

($@) infixl :: !(Image m) !(Image m) -> Image m
($@) orig mask = maskWith mask orig

maskWith :: !(Image m) !(Image m) -> Image m
maskWith orig mask = { orig & mask = Just mask }

addEdge :: ![ImageTag] ![ImageTag] !(Image m) -> Image m
addEdge fromTags toTags img=:{content = Composite c=:{ edges }} = { img & content = Composite {c & edges = 'DS'.insert ('DS'.fromList fromTags, 'DS'.fromList toTags) edges }}
addEdge fromTags toTags img                                     = img

instance tuneImage StrokeAttr      where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageStrokeAttr      attr) attribs}
instance tuneImage StrokeWidthAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageStrokeWidthAttr attr) attribs}
instance tuneImage XRadiusAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageXRadiusAttr     attr) attribs}
instance tuneImage YRadiusAttr where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageYRadiusAttr     attr) attribs}
instance tuneImage FillAttr        where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageFillAttr        attr) attribs}
instance tuneImage OpacityAttr     where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageFillOpacityAttr attr) attribs}
instance tuneImage OnClickAttr     where
  tuneImage image=:{Image | attribs} attr = {Image | image & attribs = updateOrAdd sameImageAttr (ImageOnClickAttr     attr) attribs}

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

sameImageAttr :: !(ImageAttr m) !(ImageAttr m) -> Bool
sameImageAttr a b = consNameOf a == consNameOf b

instance < (ImageAttr m) where < a b = consNameOf a < consNameOf b


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

tag :: ![ImageTag] !(Image m) -> Image m
tag ts image=:{Image | tags} = {Image | image & tags = 'DS'.union tags ('DS'.fromList ts)}

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

instance one Deg where
  one = Deg 1.0

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

instance one Rad where
  one = Rad 1.0

instance == FontDef where
  (==) fd1 fd2 = fd1.fontfamily  == fd2.fontfamily
              && fd1.fontyspan   == fd2.fontyspan
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
              && fd1.fontyspan   <  fd2.fontyspan

instance == Span where
  (==) (PxSpan     n1   ) (PxSpan     n2   ) = n1   == n2
  (==) (LookupSpan lu1  ) (LookupSpan lu2  ) = lu1  == lu2
  (==) (AddSpan    l1 r1) (AddSpan    l2 r2) = l1   == l2 && r1 == r2
  (==) (SubSpan    l1 r1) (SubSpan    l2 r2) = l1   == l2 && r1 == r2
  (==) (MulSpan    l1 r1) (MulSpan    l2 r2) = l1   == l2 && r1 == r2
  (==) (DivSpan    l1 r1) (DivSpan    l2 r2) = l1   == l2 && r1 == r2
  (==) (AbsSpan    sp1  ) (AbsSpan    sp2  ) = sp1  == sp2
  (==) (MinSpan    sps1 ) (MinSpan    sps2 ) = sps1 == sps2
  (==) (MaxSpan    sps1 ) (MaxSpan    sps2 ) = sps1 == sps2
  (==) _ _ = False

instance < Span where
  (<) (PxSpan     n1   ) (PxSpan     n2   ) = n1   < n2
  (<) (LookupSpan lu1  ) (LookupSpan lu2  ) = lu1  < lu2
  (<) (AddSpan    l1 r1) (AddSpan    l2 r2) = l1   < l2 && r1 < r2
  (<) (SubSpan    l1 r1) (SubSpan    l2 r2) = l1   < l2 && r1 < r2
  (<) (MulSpan    l1 r1) (MulSpan    l2 r2) = l1   < l2 && r1 < r2
  (<) (DivSpan    l1 r1) (DivSpan    l2 r2) = l1   < l2 && r1 < r2
  (<) (AbsSpan    sp1  ) (AbsSpan    sp2  ) = sp1  < sp2
  (<) (MinSpan    sps1 ) (MinSpan    sps2 ) = sps1 < sps2
  (<) (MaxSpan    sps1 ) (MaxSpan    sps2 ) = sps1 < sps2
  (<) _ _ = False

instance < LookupSpan where
  (<) (ColumnXSpan  ts1 n1  ) (ColumnXSpan  ts2 n2  ) = ts1 < ts2 && n1 < n2
  (<) (RowYSpan     ts1 n1  ) (RowYSpan     ts2 n2  ) = ts1 < ts2 && n1 < n2
  (<) (ImageXSpan   ts1     ) (ImageXSpan   ts2     ) = ts1 < ts2
  (<) (ImageYSpan   ts1     ) (ImageYSpan   ts2     ) = ts1 < ts2
  (<) (TextXSpan    fd1 str1) (TextXSpan    fd2 str2) = fd1 < fd2 && str1 < str2
  (<) _ _ = False

instance == LookupSpan where
  (==) (ColumnXSpan  ts1 n1  ) (ColumnXSpan  ts2 n2  ) = ts1 == ts2 && n1 == n2
  (==) (RowYSpan     ts1 n1  ) (RowYSpan     ts2 n2  ) = ts1 == ts2 && n1 == n2
  (==) (ImageXSpan   ts1     ) (ImageXSpan   ts2     ) = ts1 == ts2
  (==) (ImageYSpan   ts1     ) (ImageYSpan   ts2     ) = ts1 == ts2
  (==) (TextXSpan    fd1 str1) (TextXSpan    fd2 str2) = fd1 == fd2 && str1 == str2
  (==) _ _ = False
