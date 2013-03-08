implementation module Functor

(<$>) infixl 4 :: u:((.a -> .b) -> v:(w:(c .a) -> w:(c .b))) | Functor c, [v <= u]
(<$>) = fmap
