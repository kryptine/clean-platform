implementation module System._PointerWithFinalizer

import StdEnv
import Data.Func
import System._Pointer

:: PointerWithFinalizer = PointerWithFinalizer !Pointer !Finalizer


pointerWithFinalizer :: !Pointer !Pointer -> PointerWithFinalizer
pointerWithFinalizer ptr finalPtr = PointerWithFinalizer ptr $ make_finalizer finalPtr ptr

withPointer :: !(Pointer -> a) !PointerWithFinalizer -> (!a, !PointerWithFinalizer)
withPointer func fptr=:(PointerWithFinalizer ptr _) = (func ptr, fptr)

:: Finalizer  = {finalizer_implementation :: !FinalizerT}
:: FinalizerT = DummyFinalizer !Int !Int !Int

make_finalizer :: !Pointer !Pointer -> Finalizer
make_finalizer f v = {finalizer_implementation = fst $ make_finalizer_c f v}
where
    make_finalizer_c :: !Int !Int -> (!FinalizerT, !Int)
    make_finalizer_c f v = code {
        push_finalizers
        push_a_b 0
        pop_a 1
        build_r e__system_kFinalizer 0 3 0 0
        pop_b 3
        set_finalizers
        pushI 0
    }
