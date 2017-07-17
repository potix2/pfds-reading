{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Ex2_6(UnbalancedMap(E, T), empty, bind, lookup) where
import FiniteMap
import Prelude hiding(lookup)

data UnbalancedMap k a =
    E |
    T (UnbalancedMap k a) k a (UnbalancedMap k a)
    deriving Show

instance Ord k => FiniteMap UnbalancedMap k where
    empty = E

    bind k a E = T E k a E
    bind k a (T l k' b r) =
        if k < k' then T (bind k b l) k' b r
                  else if k > k' then T l k' b (bind k a r)
                  else T l k' b r

    lookup k E = Nothing
    lookup k (T l k' a r) =
        if k < k' then (lookup k l)
                  else if k > k' then (lookup k r)
                  else Just a
