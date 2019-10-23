implementation module Data.Real

import StdEnv

approximatelyEqual :: !Real !Real -> Bool
approximatelyEqual a b = abs (a - b) <= abs (if (abs a < abs b) a b) * Epsilon
