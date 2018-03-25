definition module Data.Generics

import StdGeneric

import Control.GenBimap
import Control.GenFMap
import Control.GenHylo
import Control.GenMap
import Control.GenMapSt
import Control.GenMonad
import Control.GenReduce
import Data.GenCompress
import Data.GenEq
import Data.GenFDomain
import Data.GenLexOrd
import Data.GenZip 
import Text.GenParse
import Text.GenPrint

fromOBJECT :: !(OBJECT x) -> x
fromCONS   :: !(CONS x)   -> x
fromRECORD :: !(RECORD x) -> x
fromFIELD  :: !(FIELD x)  -> x
fromPAIRX  :: !(PAIR x y) -> x
fromPAIRY  :: !(PAIR x y) -> y
