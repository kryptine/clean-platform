implementation module Data.Result

/*

/**
 * Return True when the argument is an Ok value and return False otherwise.
 */
isOk :: !(Result e a) -> Bool
isOk r = case r of
    Ok a -> True
    _ -> False

/**
 * Return True when the argument is an Error value and return False otherwise.
 */
isErr :: !(Result a e) -> Bool
isErr r = case r of
    Err e -> True
    _ -> False

return :: a -> Result e a
return a = Ok a

throw :: e -> Result e a
throw e = Err e

/**
 * Return the contents of an Ok value or abort at run-time with the value of Err.
 */
unwrap :: !(Result e a) -> a
unwrap r = case r of
    Ok a -> a
    Err e -> abort ("Data.Result.unwrap: result was an error")// +++ toString e)

rethrow :: !(Result e a) -> Result e b
rethrow r :== case r of
    Err e -> Err e
    Ok a -> abort "Data.Result.rethrow: result was ok"

/*
mapOk f r :== case r of
    Ok a -> Ok (f a)
    Err e -> Err e

mapErr f r :== case r of
    Err e -> Left (f e)
    Ok a -> Ok a
*/
*/
