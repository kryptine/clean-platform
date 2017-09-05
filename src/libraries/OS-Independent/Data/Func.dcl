definition module Data.Func

/**
 * Function application.
 * @type (a -> b) a -> b
 */
($) infixr 0
($) f :== f

/**
 * Function application.
 * @type a -> a
 */
app f :== f

seqSt        :: !(a .st -> .st)       ![a] !.st -> .st
mapSt        :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
fix          :: (a -> a) -> a
on           :: (b b -> c) (a -> b) -> (a a -> c)

/**
 * Completely evaluate an expression (not just to head normal form like strictness).
 */
hyperstrict  :: !.a -> .a
