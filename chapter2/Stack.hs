{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Stack(Stack(..)) where
import Prelude hiding (head, tail)

class Stack s a where
    empty   :: s a
    isEmpty :: s a -> Bool
    cons    :: a -> s a -> s a
    head    :: s a -> a
    tail    :: s a -> s a
