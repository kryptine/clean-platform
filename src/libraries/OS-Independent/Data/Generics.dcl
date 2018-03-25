definition module Data.Generics

import StdGeneric

import Data.GenEq
import Data.GenLexOrd
import Control.GenMap
import Control.GenMapSt
import Control.GenReduce
import Data.GenZip 
import Text.GenPrint
import Text.GenParse
import Data.GenCompress
import Control.GenMonad
import Control.GenHylo
import Control.GenFMap
import Control.GenBimap
import Data.GenFDomain

fromOBJECT :: !(OBJECT x) -> x
fromCONS   :: !(CONS x)   -> x
fromRECORD :: !(RECORD x) -> x
fromFIELD  :: !(FIELD x)  -> x
fromPAIRX  :: !(PAIR x y) -> x
fromPAIRY  :: !(PAIR x y) -> y
