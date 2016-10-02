definition module Data.Result

from Data.Func import abort
from Data.String import class toString(..), class +++(..), instance +++ {#Char}

:: Result e a
    = Err e
    | Ok a

:: ErrorWitness = E.e:
    { error :: e
    , describe :: !e -> String
    }

:: Usually a :== Result ErrorWitness a

/*
:: Error
    = Impossible
    | ...

:: Usually a :== Result Error a
*/

isOk r :== case r of
    Ok a -> True
    _ -> False
isErr r :== case r of
    Err e -> True
    _ -> False

//

unwrap r :== case r of
    Ok a -> a
    Err e -> abort "Data.Result.unwrap: result was an error"

unwrapErr r :== case r of
    Err e -> e
    Ok a -> abort "Data.Result.unwrapErr: result was ok"

//

return value :== Ok value
throw error :== Err {error = error, describe = toString}
rethrow r :== case r of
    Err e -> Err e
    Ok a -> abort "Data.Result.rethrow: result was ok"

panic result :== case result of
    Err {error, describe} -> abort ("Panic: " +++ describe error)
    Ok _ -> abort ("Panic: application paniced, but result was ok")
