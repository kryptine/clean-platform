implementation module Control.Monad.Fix

import Data.List
import Data.Maybe

instance MonadFix Maybe where
  mfix f =
    let a = f (unJust a) in a
    where unJust (Just x) = x
          unJust Nothing  = error "mfix Maybe: Nothing"

//instance MonadFix [] where
  //mfix f = case fix (f o head) of
             //[]    -> []
             //[x:_] -> [x : mfix (tail o f)]

