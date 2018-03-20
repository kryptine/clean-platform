definition module Data.Generics

import StdGeneric

import Data.Generics.Eq
import Data.Generics.LexOrd
import Data.Generics.Map
import Data.Generics.MapSt
import Data.Generics.Reduce
import Data.Generics.Zip 
import Data.Generics.Print
import Data.Generics.Parse
import Data.Generics.Compress
import Data.Generics.Monad
import Data.Generics.Hylo
import Data.Generics.FMap
import Data.Generics.Bimap
import Data.Generics.FDomain

fromOBJECT :: !(OBJECT x) -> x
fromCONS   :: !(CONS x)   -> x
fromRECORD :: !(RECORD x) -> x
fromFIELD  :: !(FIELD x)  -> x
fromPAIRX  :: !(PAIR x y) -> x
fromPAIRY  :: !(PAIR x y) -> y
