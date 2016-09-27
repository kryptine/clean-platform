// --------------------------------------------------------------------------
// |
// Module      :  Control.Arrow
// Copyright   :  (c) Ross Paterson 2002
// License     :  BSD-style (see the LICENSE file in the distribution)
//
// Maintainer  :  libraries@haskell.org
// Stability   :  provisional
// Portability :  portable
//
// Basic arrow definitions, based on
//
//  * /Generalising Monads to Arrows/, by John Hughes,
//    /Science of Computer Programming/ 37, pp67-111, May 2000.
//
// plus a couple of definitions ('pureA' and 'loop') from
//
//  * /A New Notation for Arrows/, by Ross Paterson, in /ICFP 2001/,
//    Firenze, Italy, pp229-240.
//
// These papers and more information on arrows can be found at
// <http://www.haskell.org/arrows/>.

implementation module Control.Arrow

import StdTuple
from StdFunc import o, const
import Data.Either
import Control.Monad.Fix
import Control.Category
import Control.Monad
import Control.Applicative


// | The basic arrow class.
//
// Instances should satisfy the following laws:
//
//  * @'arr' cid = 'id'@
//
//  * @'arr' (f >>> g) = 'arr' f >>> 'arr' g@
//
//  * @'first' ('arr' f) = 'arr' ('first' f)@
//
//  * @'first' (f >>> g) = 'first' f >>> 'first' g@
//
//  * @'first' f >>> 'arr' 'fst' = 'arr' 'fst' >>> f@
//
//  * @'first' f >>> 'arr' ('id' *** g) = 'arr' ('id' *** g) >>> 'first' f@
//
//  * @'first' ('first' f) >>> 'arr' 'assoc' = 'arr' 'assoc' >>> 'first' f@
//
// where
//
// > assoc ((a,b),c) = (a,(b,c))
//
// The other combinators have sensible default definitions,
// which may be overridden for efficiency.

class Arrow a | Category a where
    // | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

    // | Send the first component of the input through the argument
    //   arrow, and copy the rest unchanged to the output.
    first :: (a b c) -> a (b,d) (c,d)

    // | A mirror image of 'first'.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    second :: (a b c) -> a (d,b) (d,c)

    // | Split the input between the two argument arrows and combine
    //   their output.  Note that this is in general not a functor.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (***) infixr 3 :: (a b c) (a b` c`) -> a (b,b`) (c,c`)

    // | Fanout: send the input to both argument arrows and combine
    //   their output.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (&&&) infixr 3 :: (a b c) (a b c`) -> a b (c,c`)

// Ordinary functions are arrows.

instance Arrow (->) where
    arr f = f
    first x = x *** cid
    second x = cid *** x
    (***) f g = \t -> let (x,y) = t in (f x, g y)
    (&&&) f g = arr (\b -> (b,b)) >>> f *** g

// | Kleisli arrows of a monad.
:: Kleisli m a b = Kleisli (a -> m b)

runKleisli :: (Kleisli m a b) -> (a -> m b)
runKleisli (Kleisli f) = f

instance Category (Kleisli m) | Monad m where
    cid = Kleisli pure
    (O) (Kleisli f) (Kleisli g) = Kleisli (\b -> g b >>= f)

instance Arrow (Kleisli m) | Monad m where
    arr f = Kleisli (pure o f)
    first (Kleisli f) = Kleisli (\t -> let (b, d) = t in f b >>= \c -> pure (c,d))
    second (Kleisli f) = Kleisli (\t -> let (d,b) = t in f b >>= \c -> pure (d,c))
    (***) f g = first f >>> arr swap >>> first g >>> arr swap
      where swap t = let (x,y) = t in (y,x)
    (&&&) f g = arr (\b -> (b,b)) >>> f *** g

// | The identity arrow, which plays the role of 'pure' in arrow notation.
pureA :: a b b | Arrow a
pureA = arr cid

// | Precomposition with a pure function.
(^>>) infixr 1 :: (b -> c) (a c d) -> a b d | Arrow a
(^>>) f a = arr f >>> a

// | Postcomposition with a pure function.
(>>^) infixr 1 :: (a b c) (c -> d) -> a b d | Arrow a
(>>^) a f = a >>> arr f

// | Precomposition with a pure function (right-to-left variant).
(<<^) infixr 1 :: (a c d) (b -> c) -> a b d | Arrow a
(<<^) a f = a <<< arr f

// | Postcomposition with a pure function (right-to-left variant).
(^<<) infixr 1 :: (c -> d) (a b c) -> a b d | Arrow a
(^<<) f a = arr f <<< a

class ArrowZero a | Arrow a where
    zeroArrow :: a b c

// TODO
//instance ArrowZero (Kleisli m) | MonadPlus m where
    //zeroArrow = Kleisli (\_ -> mzero)

// | A monoid on arrows.
class ArrowPlus a | ArrowZero a where
    // | An associative operation with identity 'zeroArrow'.
    (<+>) infixr 5 :: (a b c) (a b c) -> a b c

// TODO
//instance ArrowPlus (Kleisli m) | MonadPlus m where
    //Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

// | Choice, for arrows that support it.  This class underlies the
// @if@ and @case@ constructs in arrow notation.
//
// Instances should satisfy the following laws:
//
//  * @'left' ('arr' f) = 'arr' ('left' f)@
//
//  * @'left' (f >>> g) = 'left' f >>> 'left' g@
//
//  * @f >>> 'arr' 'Left' = 'arr' 'Left' >>> 'left' f@
//
//  * @'left' f >>> 'arr' ('id' <+++> g) = 'arr' ('id' <+++> g) >>> 'left' f@
//
//  * @'left' ('left' f) >>> 'arr' 'assocsum' = 'arr' 'assocsum' >>> 'left' f@
//
// where
//
// > assocsum (Left (Left x)) = Left x
// > assocsum (Left (Right y)) = Right (Left y)
// > assocsum (Right z) = Right (Right z)
//
// The other combinators have sensible default definitions, which may
// be overridden for efficiency.

class ArrowChoice a | Arrow a where
    // | Feed marked inputs through the argument arrow, passing the
    //   rest through unchanged to the output.
    left :: (a b c) -> a (Either b d) (Either c d)

    // | A mirror image of 'left'.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    right :: (a b c) -> a (Either d b) (Either d c)

    // | Split the input between the two argument arrows, retagging
    //   and merging their outputs.
    //   Note that this is in general not a functor.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (<+++>) infixr 2 :: (a b c) (a b` c`) -> a (Either b b`) (Either c c`)

    // | Fanin: Split the input between the two argument arrows and
    //   merge their outputs.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (|||) infixr 2 :: (a b d) (a c d) -> a (Either b c) d

instance ArrowChoice (->) where
    left f = f <+++> cid
    right f = cid <+++> f
    (<+++>) f g = (Left o f) ||| (Right o g)
    (|||) x y = either x y

instance ArrowChoice (Kleisli m) | Monad m where
    left f = f <+++> arr cid
    right f = arr cid <+++> f
    (<+++>) f g = (f >>> arr Left) ||| (g >>> arr Right)
    (|||) (Kleisli f) (Kleisli g) = Kleisli (either f g)

// | Some arrows allow application of arrow inputs to other inputs.
// Instances should satisfy the following laws:
//
//  * @'first' ('arr' (\\x -> 'arr' (\\y -> (x,y)))) >>> 'app' = 'id'@
//
//  * @'first' ('arr' (g >>>)) >>> 'app' = 'second' g >>> 'app'@
//
//  * @'first' ('arr' (>>> h)) >>> 'app' = 'app' >>> h@
//
// Such arrows are equivalent to monads (see 'ArrowMonad').

class ArrowApply a | Arrow a where
    app :: a (a b c, b) c

instance ArrowApply (->) where
    app = \(f,x) -> f x

instance ArrowApply (Kleisli m) | Monad m where
    app = Kleisli (\(Kleisli f, x) -> f x)

// | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
//   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.

:: ArrowMonad a b = ArrowMonad (a () b)

instance Functor (ArrowMonad a) | Arrow a where
    fmap f (ArrowMonad m) = ArrowMonad (m >>> arr f)

instance Applicative (ArrowMonad a) | Arrow a where
   pure x = ArrowMonad (arr (const x))
   (<*>) (ArrowMonad f) (ArrowMonad x) = ArrowMonad (f &&& x >>> arr (uncurry cid))

instance Monad (ArrowMonad a) | ArrowApply a where
   bind (ArrowMonad m) f = ArrowMonad (
        m >>> arr (\x -> let (ArrowMonad h) = f x in (h, ())) >>> app)

instance Alternative (ArrowMonad a) | ArrowPlus a where
   empty = ArrowMonad zeroArrow
   (<|>) (ArrowMonad x) (ArrowMonad y) = ArrowMonad (x <+> y)

// TODO
//instance MonadPlus (ArrowMonad a) | ArrowApply a & ArrowPlus a where
   //mzero = ArrowMonad zeroArrow
   //ArrowMonad x `mplus` ArrowMonad y = ArrowMonad (x <+> y)

// | Any instance of 'ArrowApply' can be made into an instance of
//   'ArrowChoice' by defining 'left' = 'leftApp'.

leftApp :: (a b c) -> a (Either b d) (Either c d) | ArrowApply a
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
             (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app

// | The 'loop' operator expresses computations in which an output value
// is fed back as input, although the computation occurs only once.
// It underlies the @rec@ value recursion construct in arrow notation.
// 'loop' should satisfy the following laws:
//
// [/extension/]
//      @'loop' ('arr' f) = 'arr' (\\ b -> 'fst' ('fix' (\\ (c,d) -> f (b,d))))@
//
// [/left tightening/]
//      @'loop' ('first' h >>> f) = h >>> 'loop' f@
//
// [/right tightening/]
//      @'loop' (f >>> 'first' h) = 'loop' f >>> h@
//
// [/sliding/]
//      @'loop' (f >>> 'arr' ('id' *** k)) = 'loop' ('arr' ('id' *** k) >>> f)@
//
// [/vanishing/]
//      @'loop' ('loop' f) = 'loop' ('arr' unassoc >>> f >>> 'arr' assoc)@
//
// [/superposing/]
//      @'second' ('loop' f) = 'loop' ('arr' assoc >>> 'second' f >>> 'arr' unassoc)@
//
// where
//
// > assoc ((a,b),c) = (a,(b,c))
// > unassoc (a,(b,c)) = ((a,b),c)
//
// TODO
//class ArrowLoop a | Arrow a where
    //loop :: a (b,d) (c,d) -> a b c

//instance ArrowLoop (->) where
    //loop f b = let (c,d) = f (b,d) in c

// | Beware that for many monads (those for which the '>>=' operation
// is strict) this instance will /not/ satisfy the right-tightening law
// required by the 'ArrowLoop' class.
// TODO
//instance MonadFix m => ArrowLoop (Kleisli m) where
    //loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
      //where f' x y = f (x, snd y)
