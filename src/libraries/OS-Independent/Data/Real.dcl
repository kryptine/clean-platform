definition module Data.Real

from System._Architecture import IF_INTEL
from StdInt import IF_INT_64_OR_32

// Compile-time check whether the x86 extended precision format is used for
// intermediate values
IF_INTERMEDIATE_80BIT_REAL_PRECISION yes no :== IF_INTEL (IF_INT_64_OR_32 no yes) no

LargestReal    :== 1.7976931348623157E+308
/**
 * Smallest normalized 64 bit real > 0.0
 */
LowestReal     :== 2.2250738585072014E-308
Epsilon        :== 2.220446049250313E-16
Epsilon80      :== 1.08420217248550443E-19

approximatelyEqual :: !Real !Real -> Bool
