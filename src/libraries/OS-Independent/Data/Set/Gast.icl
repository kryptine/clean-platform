implementation module Data.Set.Gast

import Gast, Data.Set

genShow{|Set|} fx sep p x rest = genShow{|* -> *|} fx sep p (toList x) rest
