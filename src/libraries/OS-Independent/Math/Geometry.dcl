definition module Math.Geometry

/**
 * This module provides geometry related functions.
 *
 * @property-bootstrap
 *	import StdReal, StdInt
 *	derive class Gast Angle
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
 * @property normalized degree range: A.angle :: Angle:
 *	(abs deg <= toReal (maxint/365)) ==> (0.0 <=. degNorm /\ degNorm <=. 360.0)
 *	with
 *		deg     = toDeg angle
 *		degNorm = toDeg (normalize angle)
 * @property normalized radian range: A.angle :: Angle:
 *	(abs deg <= toReal (maxint/365)) ==> (0.0 <=. radNorm /\ radNorm <=. 2.0 * pi)
 *	with
 *		deg     = toDeg angle
 *		radNorm = toRad (normalize angle)
 * @property idempotence: A.angle :: Angle:
 *	(abs deg <= toReal (maxint/365)) ==> normalize angle =.= normalize (normalize angle)
 *	with
 *		deg = toDeg angle
 */
normalize :: !Angle -> Angle

instance == Angle
instance < Angle
instance + Angle
instance - Angle
instance sign Angle
instance abs Angle
