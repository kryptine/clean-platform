implementation module Graphics.Scalable

from StdMisc import abort
from StdFunc import flip
from StdTuple import fst, snd
from StdOrdList import minList, maxList
from StdOverloaded import class toReal
import Data.List
import Data.Maybe
from Data.Set import :: Set
import qualified Data.Set as DS
import Text.HTML

isPxSpan :: Span -> Bool
isPxSpan (PxSpan _) = True
isPxSpan _          = False

px :: Real -> Span
px a = PxSpan a

ex :: FontDef -> Span
ex a = LookupSpan (ExYSpan a)

descent :: FontDef -> Span
descent a = LookupSpan (DescentYSpan a)

textxspan :: FontDef String -> Span
textxspan a b = LookupSpan (TextXSpan a b)

imagexspan :: [ImageTag] -> Span
imagexspan as = LookupSpan (ImageXSpan ('DS'.fromList as))

imageyspan :: [ImageTag] -> Span
imageyspan as = LookupSpan (ImageYSpan ('DS'.fromList as))

columnspan :: [ImageTag] Int -> Span
columnspan as a = LookupSpan (ColumnXSpan ('DS'.fromList as) a)

rowspan :: [ImageTag] Int -> Span
rowspan as a = LookupSpan (RowYSpan ('DS'.fromList as) a)

instance zero Span where zero                    = PxSpan zero
instance one  Span where one                     = PxSpan one
instance +    Span where + (PxSpan a) (PxSpan b) = PxSpan (a + b)
                         + s          t          = AddSpan s t
instance -    Span where - (PxSpan a) (PxSpan b) = PxSpan (a - b)
                         - s          t          = SubSpan s t
instance abs  Span where abs (PxSpan  a)         = PxSpan  (abs a)
                         abs (AddSpan a b)       = AbsSpan (AddSpan a b)
                         abs (SubSpan a b)       = AbsSpan (SubSpan a b)
                         abs (MulSpan a k)       = AbsSpan (MulSpan a k)
                         abs (DivSpan a k)       = AbsSpan (DivSpan a k)
                         abs span                = span
instance ~    Span where ~ s                     = zero - s
instance *.   Real where *. l              r     = l * toReal r
instance *.   Span where *. (PxSpan  a)    k     = PxSpan    (a * toReal k)
                         *. (MulSpan a k1) k     = MulSpan a (toReal k * k1)
                         *. (DivSpan a k1) k     = MulSpan a (toReal k / k1)
                         *. s              k     = MulSpan s (toReal k)
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

minSpan :: [Span] -> Span
minSpan []  = zero
minSpan [a] = a
minSpan as
  | isEmpty others = min_pxs
  | isEmpty pxs    = MinSpan others
  | otherwise      = MinSpan [min_pxs : others]
  where
  (pxs, others) = partition isPxSpan as
  min_pxs       = PxSpan (minList [x \\ PxSpan x <- pxs])

maxSpan :: [Span] -> Span
maxSpan []  = zero
maxSpan [a] = a
maxSpan as
  | isEmpty others = max_pxs
  | isEmpty pxs    = MaxSpan others
  | otherwise      = MaxSpan [max_pxs : others]
  where
  (pxs, others) = partition isPxSpan as
  max_pxs       = PxSpan (maxList [x \\ PxSpan x <- pxs])

empty :: Span Span -> Image m
empty xspan yspan
	= { content   = Basic EmptyImage (maxSpan [zero,xspan], maxSpan [zero,yspan])
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

text :: FontDef String -> Image m
text font str
	= { content   = Basic (TextImage font str) (textxspan font str, font.FontDef.fontyspan)
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

xline :: Span -> Image m
xline xspan
	= line Slash xspan zero

yline :: Span -> Image m
yline yspan
	= line Slash zero yspan

line :: Slash Span Span -> Image m
line slash xspan yspan
	= { content   = Basic (LineImage slash) (abs xspan, abs yspan)
	  , attribs   = [ImageStrokeAttr      {stroke      = toSVGColor "black"}
	                ,ImageStrokeWidthAttr {strokewidth = px 1.0}
	                ,ImageFillAttr        {fill        = toSVGColor "black"}
	                ,ImageFillOpacityAttr {opacity     = 1.0}
	                ]
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

circle :: Span -> Image m
circle diameter
	= { content   = Basic CircleImage (d, d)
	  , attribs   = [ImageStrokeAttr      {stroke      = toSVGColor "black"}
	                ,ImageStrokeWidthAttr {strokewidth = px 1.0}
	                ,ImageFillAttr        {fill        = toSVGColor "black"}
	                ,ImageFillOpacityAttr {opacity     = 1.0}
	                ]
	  , transform = []
	  , tags      = 'DS'.newSet
	  }
where
	d             = maxSpan [zero,diameter]

ellipse :: Span Span -> Image m
ellipse diax diay
	= { content   = Basic EllipseImage (maxSpan [zero, diax], maxSpan [zero, diay])
	  , attribs   = [ImageStrokeAttr      {stroke      = toSVGColor "black"}
	                ,ImageStrokeWidthAttr {strokewidth = px 1.0}
	                ,ImageFillAttr        {fill        = toSVGColor "black"}
	                ,ImageFillOpacityAttr {opacity     = 1.0}
	                ]
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

rect :: Span Span -> Image m
rect xspan yspan
	= { content   = Basic RectImage (maxSpan [zero,xspan], maxSpan [zero,yspan])
	  , attribs   = [ImageStrokeAttr      {stroke      = toSVGColor "black"}
	                ,ImageStrokeWidthAttr {strokewidth = px 1.0}
	                ,ImageFillAttr        {fill        = toSVGColor "black"}
	                ,ImageFillOpacityAttr {opacity     = 1.0}
	                ]
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

polygon :: [ImageOffset] -> Image m
polygon offsets
  = { content   = Basic (PolygonImage offsets) (maxSpan (map fst offsets), maxSpan (map snd offsets))
    , attribs   = []
    , transform = []
    , tags      = 'DS'.newSet
    }

rotate :: th (Image m) -> Image m | Angle th
rotate a image=:{Image | transform = ts}
| a` == zero	= image
| otherwise		= {Image | image & transform = ts`}
where
	a`			= toDeg (normalize a)
	ts`			= case ts of
					[RotateImage angle : ts]
						= let a` = normalize (toDeg angle + toDeg a) in if (a` == zero) ts [RotateImage a` : ts]
					ts  = [RotateImage a` : ts]

fit :: Span Span (Image m) -> Image m
fit xspan yspan image=:{Image | transform = ts}
	= {Image | image & transform = ts`}
where
	xspan`		= maxSpan [zero,xspan]
	yspan`		= maxSpan [zero,yspan]
	ts`			= case ts of
					[FitImage _ _ : ts] = [FitImage xspan` yspan` : ts]
					ts                  = [FitImage xspan` yspan` : ts]

fitx :: Span (Image m) -> Image m
fitx xspan image=:{Image | transform = ts}
	= {Image | image & transform = ts`}
where
	xspan`		= maxSpan [zero,xspan]
	ts`			= case ts of
					[FitXImage _ : ts] = [FitXImage xspan` : ts]
					[FitYImage _ : ts] = [FitXImage xspan` : ts]
					ts                 = [FitXImage xspan` : ts]

fity :: Span (Image m) -> Image m
fity yspan image=:{Image | transform = ts}
	= {Image | image & transform = ts`}
where
	yspan`		= maxSpan [zero,yspan]
	ts`			= case ts of
					[FitXImage _ : ts] = [FitYImage yspan` : ts]
					[FitYImage _ : ts] = [FitYImage yspan` : ts]
					ts                 = [FitYImage yspan` : ts]

applyTransforms :: [ImageTransform] ImageSpan -> ImageSpan
applyTransforms ts sp = foldr f sp ts
  where
  f (RotateImage th)   accSp           = fst (rotatedImageSpanAndOriginOffset th accSp)
  f (SkewXImage th)    accSp=:(_, ysp) = (skewXImageWidth th accSp, ysp)
  f (SkewYImage th)    accSp=:(xsp, _) = (xsp, skewYImageHeight th accSp)
  f (FitImage xsp ysp) _               = (xsp, ysp)
  f (FitXImage sp)     (_, ysp)        = (sp, ysp)
  f (FitYImage sp)     (xsp, _)        = (xsp, sp)

skewXImageWidth :: th (a, a) -> a | Angle th & span a
skewXImageWidth angle (xspan, yspan) = xspan + (abs (yspan *. tan (toReal (toRad angle))))

skewYImageHeight :: th (a, a) -> a | Angle th & span a
skewYImageHeight angle (xspan, yspan) = yspan + (abs (xspan *. tan (toReal (toRad (angle)))))

rotatedImageSpanAndOriginOffset :: th (a, a) -> ((a, a), (a, a)) | Angle th & span a
rotatedImageSpanAndOriginOffset angle (xspan, yspan)
  = (( abs (maxAllX - minAllX)
     , abs (maxAllY - minAllY) )
    , ( maxAllX - fst tTopLeft
      , maxAllY - snd tTopLeft)
    )
  where
  cx        = xspan /. 2.0
  cy        = yspan /. 2.0
  tTopLeft  = mkTransform zero zero
  allPoints = [ tTopLeft
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
  mkTransform x y = ( ((x - cx) *. cos angle`) - ((y - cy) *. sin angle`)
                    , ((x - cx) *. sin angle`) + ((y - cy) *. cos angle`))

skewx :: th (Image m) -> Image m | Angle th
skewx xskew image=:{Image | transform = ts}
| xskew` == zero	= image
| otherwise			= {Image | image & transform = ts`}
where
	xskew`			= toDeg (normalize xskew)
	ts`				= case ts of
						[SkewXImage a : ts] = let a` = normalize (a + toDeg xskew) in if (a` == zero) ts [SkewXImage a` : ts]
						ts                  = [SkewXImage xskew` : ts]

skewy :: th (Image m) -> Image m | Angle th
skewy yskew image=:{Image | transform = ts}
| yskew` == zero	= image
| otherwise			= {Image | image & transform = ts`}
where
	yskew`			= toDeg (normalize yskew)
	ts`				= case ts of
						[SkewYImage a : ts] = let a` = normalize (a + toDeg yskew) in if (a` == zero) ts [SkewYImage a` : ts]
						ts                  = [SkewYImage yskew` : ts]

radian :: Real -> Rad
radian r = Rad r

degree :: Real -> Deg
degree d = Deg d

pi =: 3.14159265359

overlay :: [ImageAlign] [ImageOffset] [Image m] (Host m) -> Image m
overlay _ _ [] host
	= case host of
		Just img = img
		nothing  = empty zero zero
overlay aligns offsets imgs host
	= { content   = Composite { offsets = take l offsets
                              , host    = host
                              , compose = AsOverlay (take l aligns) imgs }
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }
  where
  l = length imgs

beside :: [YAlign] [ImageOffset] [Image m] (Host m) -> Image m
beside ylayouts offsets imgs host
  = grid (Rows 1) (LeftToRight,TopToBottom) (take l [(AtLeft,ylayout) \\ ylayout <- ylayouts]) (take l offsets) imgs host
  where
  l = length imgs

above :: [XAlign] [ImageOffset] [Image m] (Host m) -> Image m
above xlayouts offsets imgs host
  = grid (Columns 1) (LeftToRight,TopToBottom) (take l [(xlayout,AtTop) \\ xlayout <- xlayouts]) (take l offsets) imgs host
  where
  l = length imgs

grid :: GridDimension GridLayout [ImageAlign] [ImageOffset] [Image m] (Host m) -> Image m
grid _ _ _ _ [] host
	= case host of
	    Just img = img
	    nothing  = empty zero zero
grid dimension layout aligns offsets imgs host
	= { content   = Composite { offsets = take nr_of_imgs offsets
                              , host    = host
                              , compose = AsGrid (cols, rows) (take nr_of_imgs aligns) imgs`
                              }
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }
where
	nr_of_imgs    = length imgs
	(cols,rows)   = case dimension of
	                   Rows    nr = let nr` = max 1 nr
	                                 in (nr_of_imgs / nr` + sign (nr_of_imgs rem nr`),nr`)
	                   Columns nr = let nr` = max 1 nr
	                                 in (nr`,nr_of_imgs / nr` + sign (nr_of_imgs rem nr`))
	imgs_complete = imgs ++ repeatn (cols*rows-nr_of_imgs) (empty zero zero)
	imgs`         = arrange_layout layout (if (is_row_major dimension) 
	                                         (chop cols imgs_complete) 
	                                         [map (flip (!!) i) (chop rows imgs_complete) \\ i <- [0..rows-1]]
	                                         )
	
	is_row_major :: GridDimension -> Bool
	is_row_major (Rows _) = True
	is_row_major _        = False
	
	arrange_layout :: GridLayout [[a]] -> [[a]]
	arrange_layout (LeftToRight, TopToBottom) xs = xs
	arrange_layout (RightToLeft, TopToBottom) xs = map reverse xs
	arrange_layout (LeftToRight, BottomToTop) xs = reverse xs
	arrange_layout (RightToLeft, BottomToTop) xs = reverse (map reverse xs)

collage :: [ImageOffset] [Image m] (Host m) -> Image m
collage offsets imgs host
	= { content   = Composite { offsets = take (length imgs) offsets, host = host, compose = AsCollage imgs}
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

instance tune_image StrokeAttr      where
  tune_image image=:{Image | attribs} attr = {Image | image & attribs = update_or_add sameImageAttr (ImageStrokeAttr      attr) attribs}
instance tune_image StrokeWidthAttr where
  tune_image image=:{Image | attribs} attr = {Image | image & attribs = update_or_add sameImageAttr (ImageStrokeWidthAttr attr) attribs}
instance tune_image FillAttr        where
  tune_image image=:{Image | attribs} attr = {Image | image & attribs = update_or_add sameImageAttr (ImageFillAttr        attr) attribs}
instance tune_image OpacityAttr     where
  tune_image image=:{Image | attribs} attr = {Image | image & attribs = update_or_add sameImageAttr (ImageFillOpacityAttr attr) attribs}
instance tune_image OnClickAttr     where
  tune_image image=:{Image | attribs} attr = {Image | image & attribs = update_or_add sameImageAttr (ImageOnClickAttr     attr) attribs}

(<@<) infixl 2 :: (Image m) (attr m) -> Image m | tune_image attr
(<@<) image attr = tune_image image attr

(>@>) infixr 2 :: (attr m) (Image m) -> Image m | tune_image attr
(>@>) attr image = tune_image image attr

consNameOf :: (ImageAttr m) -> String
consNameOf (ImageStrokeAttr      _) = "ImageStrokeAttr"
consNameOf (ImageStrokeWidthAttr _) = "ImageStrokeWidthAttr"
consNameOf (ImageFillAttr        _) = "ImageFillAttr"
consNameOf (ImageFillOpacityAttr _) = "ImageFillOpacityAttr"
consNameOf (ImageOnClickAttr     _) = "ImageOnClickAttr"

sameImageAttr :: (ImageAttr m) (ImageAttr m) -> Bool
sameImageAttr a b = consNameOf a == consNameOf b

instance < (ImageAttr m) where < a b = consNameOf a < consNameOf b


instance toSVGColor String where toSVGColor name = SVGColorText name
instance toSVGColor RGB    where toSVGColor {RGB | r,g,b} = SVGRGB r g b

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

tag :: [ImageTag] (Image m) -> Image m
tag ts image=:{Image | tags} = {Image | image & tags = 'DS'.union tags ('DS'.fromList ts)}

tags :: (Image m) -> [ImageTag]
tags image=:{Image | tags} = 'DS'.toList tags

/** update_or_add c x xs = ys:
		@xs must be a sorted list. @ys replaces the first element y in @xs for which (@c @x y) is valid with @x.
		If such an element is not found, then @x is added to @xs.
		@ys is also a sorted list if @c respects the ordering relation.
*/
update_or_add :: (a a -> Bool) a [a] -> [a] | < a
update_or_add c x [] = [x]
update_or_add c x yys=:[y:ys]
| y < x		= [y : update_or_add c x ys]
| c x y		= [x : ys]
| otherwise	= [x : yys]

/** chop n xs = xss:
        @xss consists of the subsequent sub-lists of @xs of length @n.
        The length of the last element of @xss can be less than @n.
*/
chop :: Int [a] -> [[a]]
chop n [] = []
chop n xs = [first_n : chop n without_n]
where
	(first_n,without_n) = splitAt n xs

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
