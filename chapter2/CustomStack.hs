{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module CustomStack(CustomStack(..), empty, isEmpty, cons, head, tail) where
import Stack
import Prelude hiding (head, tail)

data CustomStack a =
    Nil |
    Cons a (CustomStack a)
    deriving Show

instance Stack CustomStack a where
    empty         = Nil
    isEmpty Nil   = True
    isEmpty _     = False
    cons h t      = Cons h t
    head (Cons h t) = h
    tail (Cons h t) = t
