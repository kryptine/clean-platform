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

/**
 * Apply a state function to a list of values.
 * See also {{`mapSt`}}.
 *
 * @param The function.
 * @param The list of values.
 * @param The initial state.
 * @result The final state.
 */
seqSt        :: !(a .st -> .st)       ![a] !.st -> .st

/**
 * Apply a state function to a list of values and return the results.
 * See also {{`seqSt`}}.
 *
 * @param The function.
 * @param The list of values.
 * @param The initial state.
 * @result The value results and the final state.
 */
mapSt        :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
/**
 * The fixed point combinator, reducing `fix f` to `f (fix f)`.
 */
fix          :: !(a -> a) -> a

/**
 * Apply a binary function on another domain.
 *
 * Typical usage: `sortBy (on (<) toInt) :: [a] -> [a] | toInt a`
 * Or infix: `sortBy ((<) `on` toInt) :: [a] -> [a] | toInt a`
 */
on           :: (b b -> c) (a -> b) -> (a a -> c)

/**
 * Infix version of {{`on`}}.
 * @type (b b -> c) (a -> b) -> (a a -> c)
 */
(`on`) infixl 0
(`on`) :== on

/**
 * Completely evaluate an expression (not just to head normal form like strictness).
 */
hyperstrict  :: !.a -> .a
