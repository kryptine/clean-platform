definition module Data.Result

from Data.Func import abort

:: Result e a
    = Err e
    | Ok a

:: Error
    = Impossible
    | ...

:: Usually a :== Result Error a

isOk r :== case r of
    Ok a -> True
    _ -> False
isErr r :== case r of
    Err e -> True
    _ -> False

return a :== Ok a
throw e :== Err e

unwrap r :== case r of
    Ok a -> a
    Err e -> abort ("Data.Result.unwrap: result was an error")// +++ toString e)

rethrow r :== case r of
    Err e -> Err e
    Ok a -> abort "Data.Result.rethrow: result was ok"
