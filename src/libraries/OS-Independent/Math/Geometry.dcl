definition module Math.Geometry

/**
 * This module provides geometry related functions.
 *
 * @property-bootstrap
 *	import StdBool, StdReal
 *	derive class Gast Angle
 *	derive bimap []
 *
 *	(~~) infix 4
 *	(~~) x y :== approxEqual (Deg 1.0E-100) x y
 */

from StdEnv import class ==, class <, class +, class -, class sign, class abs

//* The constant pi.
pi :== 3.14159265359

/**
 * An angle.
 */
:: Angle
  = Deg !Real //* An angle in degree representation
  | Rad !Real //* An angle in radian representation

/**
 * An angle corresponding to the given radian representation.
 *
 * @param the radian representation
 * @result the corresponding angle
 */
rad :: !Real -> Angle

/**
 * An angle corresponding to the given degree representation.
 *
 * @param the degree representation
 * @result the corresponding angle
 */
deg :: !Real -> Angle

/**
 * The radian representation of a given angle.
 *
 * @param the angle
 * @result the angle's radian representation
 */
toDeg     :: !Angle -> Real
toRad     :: !Angle -> Real

/**
 * Normalizes an angle.
 *
 * @param the angle to normalize
 * @result the normalized angle
 * @property range_deg: A.angle :: Angle:
 *	let deg = toDeg (normalize angle)
 *	in 0.0 <= deg && deg <= 360.0
 * @property range_rad: A.angle :: Angle:
 *	let rad = toRad (normalize angle)
 *	in 0.0 <= rad && rad <= 2.0 * pi
 * @property idempotence: A.angle :: Angle:
 *   normalize angle ~~ normalize (normalize angle)
 */
normalize :: !Angle -> Angle

instance == Angle
instance < Angle
instance + Angle
instance - Angle
instance sign Angle
instance abs Angle
