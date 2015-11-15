module Memo where

import Data.Map as M
-- import Control.Monad.Identity
import Control.Monad.State.Lazy

class Memo c where
    memo :: (k -> (v, c)) -> k -> (v, c)
    upMemo :: (k -> v) -> k -> (v, c)

    {-
    
    do
        cache <- get
        case input `M.lookup` cache of
            Nothing ->  let result = computation input
                        in do
                            put cache
                            result
            Just value -> return value
    -}

data Cached k v = Cached (M.Map k v) v

{-

instance Memo (Cache k v) where
    memo computation input = undefined
    upMemo = ((\v -> (v, cacheEmpty)) .)

c = Cache M.empty
f :: (Integer, Cache Integer Integer)
f = memo (upMemo $ \x -> x * x) 3
-}

instance Monad (Cached k) where
    (Cached map v) >>= f = undefined
    return = Cached M.empty

{-

f = (k, c) -> (v, c)



-}