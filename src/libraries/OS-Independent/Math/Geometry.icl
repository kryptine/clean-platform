implementation module Math.Geometry

import StdOverloaded, StdClass, StdReal

pi =: 3.14159265359

rad :: !Real -> Angle
rad r = Rad r

deg :: !Real -> Angle
deg d = Deg d

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

