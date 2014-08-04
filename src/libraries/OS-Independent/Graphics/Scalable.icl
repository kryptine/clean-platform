implementation module Graphics.Scalable

from StdMisc import abort
from StdFunc import flip
from StdTuple import fst, snd
from StdOrdList import minList, maxList
import Data.List
import Data.Maybe
from Data.Set import :: Set
import qualified Data.Set as DS
import Text.HTML

:: Slash
	= Slash | Backslash

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

imagexspan :: (Set ImageTag) -> Span
imagexspan as = LookupSpan (ImageXSpan as)

imageyspan :: (Set ImageTag) -> Span
imageyspan as = LookupSpan (ImageYSpan as)

columnspan :: (Set ImageTag) Int -> Span
columnspan as a = LookupSpan (ColumnXSpan as a)

rowspan :: (Set ImageTag) Int -> Span
rowspan as a = LookupSpan (RowYSpan as a)

instance zero Span where zero                    = PxSpan zero
instance one  Span where one                     = PxSpan one
instance +    Span where + (PxSpan a) (PxSpan b) = PxSpan (a+b)
                         + s          t          = AddSpan s t
instance -    Span where - (PxSpan a) (PxSpan b) = PxSpan (a-b)
                         - s          t          = SubSpan s t
instance abs  Span where abs (PxSpan  a)         = PxSpan  (abs a)
                         abs (AddSpan a b)       = AbsSpan (AddSpan a b)
                         abs (SubSpan a b)       = AbsSpan (SubSpan a b)
                         abs (MulSpan a k)       = AbsSpan (MulSpan a k)
                         abs (DivSpan a k)       = AbsSpan (DivSpan a k)
                         abs span                = span
instance ~    Span where ~ s                     = zero - s           
instance *.   Int  where *. s k                  = s *. (toReal k)
instance /.   Int  where /. s k                  = s /. (toReal k)
instance *.   Real where *. (PxSpan  a)    k     = PxSpan    (a*k)
                         *. (MulSpan a k1) k     = MulSpan a (k*k1)
                         *. (DivSpan a k1) k     = MulSpan a (k/k1)
                         *. s              k     = MulSpan s k
instance /.   Real where /. (PxSpan  a)    k     = PxSpan    (a/k)
                         /. (MulSpan a k1) k     = MulSpan a (k1/k)
                         /. (DivSpan a k1) k     = DivSpan a (k1*k)
                         /. s              k     = DivSpan s k

minSpan :: [Span] -> Span
minSpan []			= zero
minSpan [a]			= a
minSpan as
| isEmpty others	= min_pxs
| isEmpty pxs		= MinSpan others
| otherwise			= MinSpan [min_pxs : others]
where
	(pxs,others)	= spanfilter isPxSpan as
	min_pxs			= PxSpan (minList [x \\ PxSpan x <- pxs])

maxSpan :: [Span] -> Span
maxSpan []			= zero
maxSpan [a]			= a
maxSpan as
| isEmpty others	= max_pxs
| isEmpty pxs		= MaxSpan others
| otherwise			= MaxSpan [max_pxs : others]
where
	(pxs,others)	= spanfilter isPxSpan as
	max_pxs			= PxSpan (maxList [x \\ PxSpan x <- pxs])


empty :: Span Span -> Image m
empty xspan yspan
	= { content   = Basic EmptyImage {xspan=maxSpan [zero,xspan],yspan=maxSpan [zero,yspan]}
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

text :: FontDef String -> Image m
text font str
	= { content   = Basic (TextImage font str) {xspan=textxspan font str,yspan=font.FontDef.fontyspan}
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
	= { content   = Basic (LineImage slash) {xspan=abs xspan,yspan=abs yspan}
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
	= { content   = Basic CircleImage {xspan=d,yspan=d}
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
	= { content   = Basic EllipseImage {xspan=maxSpan [zero, diax],yspan=maxSpan [zero, diay]}
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
	= { content   = Basic RectImage {xspan=maxSpan [zero,xspan],yspan=maxSpan [zero,yspan]}
	  , attribs   = [ImageStrokeAttr      {stroke      = toSVGColor "black"}
	                ,ImageStrokeWidthAttr {strokewidth = px 1.0}
	                ,ImageFillAttr        {fill        = toSVGColor "black"}
	                ,ImageFillOpacityAttr {opacity     = 1.0}
	                ]
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

rotate :: ImageAngle (Image m) -> Image m
rotate a image=:{Image | transform = ts}
| a` == zero	= image
| otherwise		= {Image | image & transform = ts`}
where
	a`			= normalize_angle a
	ts`			= case ts of
					[RotateImage angle : ts]
						= let a` = normalize_angle (angle + a) in if (a` == zero) ts [RotateImage a` : ts]
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
  f (RotateImage th)   accSp          = rotatedImageSpan accSp th
  f (SkewXImage th)    accSp=:{xspan} = {accSp & xspan = skewXImageWidth accSp th}
  f (SkewYImage th)    accSp=:{yspan} = {accSp & yspan = skewYImageHeight accSp th}
  f (FitImage xsp ysp) accSp          = {ImageSpan | xspan = xsp,   yspan = ysp}
  f (FitXImage sp)     {yspan}        = {ImageSpan | xspan = sp,    yspan = yspan}
  f (FitYImage sp)     {xspan}        = {ImageSpan | xspan = xspan, yspan = sp}

skewXImageWidth :: ImageSpan ImageAngle -> Span
skewXImageWidth {xspan, yspan} angle = xspan + (AbsSpan (yspan *. tan angle))

skewYImageHeight :: ImageSpan ImageAngle -> Span
skewYImageHeight {xspan, yspan} angle = yspan + (AbsSpan (xspan *. tan angle))

rotatedImageSpan :: ImageSpan ImageAngle -> ImageSpan
rotatedImageSpan {xspan, yspan} angle
  = { xspan = AbsSpan (maxSpan allX - minSpan allX)
    , yspan = AbsSpan (maxSpan allY - minSpan allY) }
  where
  cx = xspan /. 2.0
  cy = yspan /. 2.0
  allPoints = [ mkTransform (px 0.0) (px 0.0)
              , mkTransform xspan    (px 0.0)
              , mkTransform (px 0.0) yspan
              , mkTransform xspan    yspan ]
  allX = map fst allPoints
  allY = map snd allPoints
  mkTransform x y = ( cx + (x - cx) *. cos angle + (y - cy) *. sin angle
                    , cy - (x - cx) *. sin angle + (y - cy) *. cos angle)

skewx :: ImageAngle (Image m) -> Image m
skewx xskew image=:{Image | transform = ts}
| xskew` == zero	= image
| otherwise			= {Image | image & transform = ts`}
where
	xskew`			= normalize_angle xskew
	ts`				= case ts of
						[SkewXImage a : ts] = let a` = normalize_angle (a + xskew) in if (a` == zero) ts [SkewXImage a` : ts]
						ts                  = [SkewXImage xskew` : ts]

skewy :: ImageAngle (Image m) -> Image m
skewy yskew image=:{Image | transform = ts}
| yskew` == zero	= image
| otherwise			= {Image | image & transform = ts`}
where
	yskew`			= normalize_angle yskew
	ts`				= case ts of
						[SkewYImage a : ts] = let a` = normalize_angle (a + yskew) in if (a` == zero) ts [SkewYImage a` : ts]
						ts                  = [SkewYImage yskew` : ts]

normalize_angle :: Real -> Real
normalize_angle a
| a` <= 360.0	= a
| a  > 0.0		= a - d
| otherwise		= a + d
where
	a`			= abs a
	d			= toReal (entier (a` / 360.0)) * 360.0

radian :: Real -> ImageAngle
radian r = r / (pi / 180.0)

degree :: Real -> ImageAngle
degree d = d

pi =: 3.1415926

overlay :: [ImageAlign] [ImageOffset] [Image m] (Host m) -> Image m
overlay _ _ [] host
	= case host of
		Just img = img
		nothing  = empty zero zero
overlay aligns offsets imgs host
	= { content   = Composite { offsets = offsets, host = host, compose = AsOverlay aligns imgs }
	  , attribs   = []
	  , transform = []
	  , tags      = 'DS'.newSet
	  }

beside :: [YAlign] [ImageOffset] [Image m] (Host m) -> Image m
beside ylayouts offsets imgs host
	= grid (Rows 1) (LeftToRight,TopToBottom) [(AtLeft,ylayout) \\ ylayout <- ylayouts] offsets imgs host

above :: [XAlign] [ImageOffset] [Image m] (Host m) -> Image m
above xlayouts offsets imgs host
	= grid (Columns 1) (LeftToRight,TopToBottom) [(xlayout,AtTop) \\ xlayout <- xlayouts] offsets imgs host

grid :: GridDimension GridLayout [ImageAlign] [ImageOffset] [Image m] (Host m) -> Image m
grid _ _ _ _ [] host
	= case host of
	    Just img = img
	    nothing  = empty zero zero
grid dimension layout aligns offsets imgs host
	= { content   = Composite { offsets = offsets, host = host, compose = AsGrid (cols, rows) aligns imgs` }
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
	                                         (partition cols imgs_complete) 
	                                         [map (flip (!!) i) (partition rows imgs_complete) \\ i <- [0..rows-1]]
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
	= { content   = Composite { offsets = offsets, host = host, compose = AsCollage imgs}
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

tag :: (Set ImageTag) (Image m) -> Image m
tag ts image=:{Image | tags} = {Image | image & tags = 'DS'.union tags ts}

tags :: (Image m) -> (Set ImageTag)
tags image=:{Image | tags} = tags

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

/** partition n xs = xss:
        @xss consists of the subsequent sub-lists of @xs of length @n.
        The length of the last element of @xss can be less than @n.
*/
partition :: Int [a] -> [[a]]
partition n [] = []
partition n xs = [first_n : partition n without_n]
where
	(first_n,without_n) = splitAt n xs

/** spanfilter c xs = (yes,no):
		@yes is (filter @c @xs), and @no is (filter (not o @c) @xs).
*/
spanfilter :: (a -> Bool) [a] -> ([a],[a])
spanfilter c [] = ([],[])
spanfilter c [x:xs]
| c x			= ([x:yes],no)
| otherwise		= (yes,[x:no])
where
	(yes,no)	= spanfilter c xs

instance + ImageOffset where
  (+) (xal1, yal1) (xal2, yal2) = (xal1 + xal2, yal1 + yal2)
