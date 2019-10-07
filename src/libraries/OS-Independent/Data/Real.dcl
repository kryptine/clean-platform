definition module Data.Real

from System._Architecture import IF_INTEL
from StdInt import IF_INT_64_OR_32

// Compile-time check whether the x86 extended precision format is used for
// intermediate values
IF_INTERMEDIATE_80BIT_REAL_PRECISION yes no :== IF_INTEL (IF_INT_64_OR_32 no yes) no

LargestReal    :== 1.79769313486231571E+308
SmallestReal   :== 2.22507385850720138E-308
Epsilon        :== 2.22044604925031308E-16
LargestReal80  :== 1.18973149535723177E+4932
SmallestReal80 :== 3.36210314311209351E-4932
Epsilon80      :== 1.08420217248550443E-19
